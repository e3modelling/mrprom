#' calcIMaxResPot
#'
#' Use res max potential data from the "EUROPEAN COMMISSION", "GlobalWindAtlas"
#' and "MENA_EDS" model data, to derive OPENPROM input parameter iMaxResPot.
#'
#' @return  OPENPROM input data iMaxResPot
#' The output data for wind potential calculated from the "GlobalWindAtlas".
#' The output data for EU countries (solar and biomass) calculated from the
#' "EUROPEAN COMMISSION".
#' The output data for middle East and north Africa calculated from "MENA_EDS"
#' model. Countries with NA is used the region or global mean value.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IMaxResPot", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% filter select mutate group_by
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom tidyr expand_grid
#' @importFrom R.utils isZero

calcIMaxResPot <- function() {

  # The data has information about res max potential of EU countries from the
  # EUROPEAN COMMISSION.
  a1 <- readSource("EU_COM_RES")
  
  #add countries of MENA
  a2 <- readSource("MENA_EDS", subtype =  "POTRENMAX")

  q1 <- as.quitte(a1)
  q2 <- as.quitte(a2)

  PGRENEF <- toolGetMapping(paste0("PGRENEF.csv"),
                         type = "blabla_export",
                         where = "mrprom")
  
  PGRENEF <- as.character(PGRENEF[, 1])
  
  q4 <- readSource("GlobalWindAtlas")
  q4 <- as.quitte(q4)

  #rename the variables
  q1[["variable"]] <- ifelse(q1[["variable"]] == "wind onshore", PGRENEF[1], as.character(q1[["variable"]]))
  q1[["variable"]] <- ifelse(q1[["variable"]] == "wind offsore", PGRENEF[6], as.character(q1[["variable"]]))
  q1[["variable"]] <- ifelse(q1[["variable"]] == "solar", PGRENEF[2], as.character(q1[["variable"]]))
  q1[["variable"]] <- ifelse(q1[["variable"]] == "biomass", PGRENEF[3], as.character(q1[["variable"]]))
  q4[["variable"]] <- ifelse(q4[["variable"]] == "WindPotential", PGRENEF[1], as.character(q4[["variable"]]))

  #30% is wind offshore
  q5 <- q4
  q5[["value"]] <- q5[["value"]] * 0.3
  q5[["variable"]] <- ifelse(q5[["variable"]] == PGRENEF[1], PGRENEF[6], as.character(q5[["variable"]]))
  
  #Landlocked_Countries put WNO to 0
  Landlocked_Countries <- readSource("Landlocked_Countries")
  Landlocked_Countries <- as.quitte(Landlocked_Countries)
  Landlocked_Countries <- Landlocked_Countries %>% select(region, value)
  
  q5 <- left_join(q5, Landlocked_Countries, by = c("region"))
  
  q5[which(q5[["value.y"]] == 1), 7] <- 0
  names(q5) <- sub("value.x", "value", names(q5))
  q5 <- q5 %>% select(- value.y)
  
  #70% is wind onshore
  q4[["value"]] <- q4[["value"]] * 0.7
  
  q1 <- as.quitte(q1) %>%
    interpolate_missing_periods(period = 2010:2050, expand.values = TRUE)
  
  q4 <- as.quitte(q4) %>%
    interpolate_missing_periods(period = 2010:2050, expand.values = TRUE)
  
  q5 <- as.quitte(q5) %>%
    interpolate_missing_periods(period = 2010:2050, expand.values = TRUE)
  
  #add WND
  q1 <- full_join(q4, q1, by = c("variable", "region", "period", "model", "scenario", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c(value.y, value.x))
  
  #add WNO
  q1 <- full_join(q5, q1, by = c("variable", "region", "period", "model", "scenario", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c(value.y, value.x))

  q2["variable"] <- q2["PGRENEF"]
  q2 <- select(q2, -c("PGRENEF"))

  #drop countries with NA
  region <- NULL
  q2 <- filter(q2, !(region %in% c("LIB", "SYR", "TUR")))

  #rename the countries
  q2[["region"]] <- sub("MOR", "MAR", q2[["region"]])
  q2[["region"]] <- sub("ALG", "DZA", q2[["region"]])
  q2[["region"]] <- sub("LIB", "LBY", q2[["region"]])
  q2[["region"]] <- sub("LEB", "LBN", q2[["region"]])

  q2 <- as.data.frame(q2)
  
  value <- NULL
  variable <- NULL
  
  q2[q2 == 0] <- NA
           
  q2 <- q2 %>% group_by(variable, region) %>% 
    mutate(value = ifelse(is.na(value), mean(value , na.rm = TRUE), value))
  
  q2 <- as.data.frame(q2)
  
  #add the rest of variables with NA value
  x <- as.data.frame(expand_grid(unique(PGRENEF), unique(q1["region"]), unique(q1["period"])))
  names(x)[1] <- "variable"
  x["valuenew"] <- NA
  x["model"] <- q1[1, 1]
  x["scenario"] <- q1[1, 2]
  x["unit"] <- "GW"

  valuenew <- NULL
  z <- left_join(x, q1, by = c("variable", "region", "period", "model", "scenario", "unit")) %>%
    mutate(value = ifelse(is.na(valuenew), value, valuenew)) %>%
    select(-valuenew)

  z <- as.quitte(z)

  q2["unit"] <- "GW"

  q2 <- as.quitte(q2) %>%
    interpolate_missing_periods(period = 2010:2050, expand.values = TRUE)

  qx <- left_join(z, q2, by = c("variable", "region", "period", "model", "scenario", "unit")) %>%
    mutate(value = ifelse(is.na(value.x) & value.y > 0, value.y, value.x)) %>%
    select(-c(value.x, value.y))

  qx <- as.quitte(qx)
  qx_bu <- qx
  
  # assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  names(qx) <- sub("region", "CountryCode", names(qx))
  ## add h12 mapping to dataset
  qx <- left_join(qx, h12, by = "CountryCode")
  ## add new column containing regional mean value
  value <- NULL
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "variable"))
  names(qx) <- sub("CountryCode", "region", names(qx))
  qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  qx_bu <- select(qx_bu, -c("model", "scenario"))
  ## assign to countries with NA, their H12 region mean
  value.x <- NULL
  value.y <- NULL
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  ## assign to countries that still have NA, the global mean
  qx_bu <- qx
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "variable"))
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  qx <- as.quitte(qx) %>%
    interpolate_missing_periods(period = 2010:2100, expand.values = TRUE)
  
  x <- as.magpie(qx)

  return(list(x = x,
              weight = NULL,
              unit = "GW",
              description = "EUROPEAN COMMISSION and MENA_EDS model"))

}
