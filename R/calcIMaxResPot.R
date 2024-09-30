#' calcIMaxResPot
#'
#' Use res max potential data from the "EUROPEAN COMMISSION" and MENA_EDS model data,
#' to derive OPENPROM input parameter iMaxResPot.
#'
#' @return  OPENPROM input data iMaxResPot
#' The output data for EU countries calculated from the "EUROPEAN COMMISSION".
#' The output data for middle East and north Africa calculated from MENA_EDS
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

  PGRENEF <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "PGRENEF")
  PGRENEF <- unlist(strsplit(PGRENEF[, 1], ","))

  #rename the variables
  q1[["variable"]] <- ifelse(q1[["variable"]] == "wind onshore", PGRENEF[3], as.character(q1[["variable"]]))
  q1[["variable"]] <- ifelse(q1[["variable"]] == "wind offsore", PGRENEF[4], as.character(q1[["variable"]]))
  q1[["variable"]] <- ifelse(q1[["variable"]] == "solar", PGRENEF[5], as.character(q1[["variable"]]))
  q1[["variable"]] <- ifelse(q1[["variable"]] == "biomass", PGRENEF[7], as.character(q1[["variable"]]))

  q1 <- as.quitte(q1) %>%
    interpolate_missing_periods(period = 2010:2050, expand.values = TRUE)

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
  
  # assign to countries with NA, their H12 region with weights
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  
  ## assign to countries with NA, their H12 region with weights calculated from population
  population <- calcOutput(type = "POP", aggregate = FALSE)
  population <- as.quitte(population)
  
  # compute weights by population
  names(population) <- sub("region", "CountryCode", names(population))
  
  ## add mapping to population
  population <- left_join(population, h12, by = "CountryCode")
  value.x <- NULL
  value.y <- NULL
  weights <- NULL
  value <- NULL
  POP <- mutate(population, weights = sum(value, na.rm = TRUE), .by = c("RegionCode", "period"))
  POP["weights"] <- POP["value"] / POP["weights"]
  
  names(POP) <- sub("CountryCode", "region", names(POP))
  POP <- select(POP, -c("value", "model", "scenario", "X", "variable", "unit"))
  qx <- left_join(qx, POP, by = c("region", "period"))
  
  qx <- mutate(qx, value = sum(value, na.rm = TRUE), .by = c("RegionCode", "period", "variable", "unit"))
  
  qx["value"] <- qx["value"] * qx["weights"]
  
  qx <- select(qx, -c("weights"))
  
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit", "model", "scenario")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y", "RegionCode"))
  
  qx[qx == 0] <- NA
  
  ## assign to countries that still have NA, the global with weights
  qx_bu <- qx
  # compute weights by population
  POP <- mutate(population, weights = sum(value, na.rm = TRUE), .by = c("period"))
  POP["weights"] <- POP["value"] / POP["weights"]
  names(POP) <- sub("CountryCode", "region", names(POP))
  POP <- select(POP, -c("value", "model", "scenario", "X", "RegionCode", "variable", "unit"))
  qx <- left_join(qx, POP, by = c("region", "period"))
  
  qx <- mutate(qx, value = sum(value, na.rm = TRUE), .by = c("period", "variable", "unit"))
  
  qx["value"] <- qx["value"] * qx["weights"]
  
  qx <- select(qx, -c("weights"))
  
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit", "model", "scenario")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  x <- as.quitte(qx) %>% as.magpie()
  
  qx <- as.quitte(qx) %>%
    interpolate_missing_periods(period = 2010:2100, expand.values = TRUE)

  x <- as.magpie(qx)

  return(list(x = x,
              weight = NULL,
              unit = "GW",
              description = "EUROPEAN COMMISSION and MENA_EDS model"))

}
