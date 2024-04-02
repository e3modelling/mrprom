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
#' @importFrom dplyr %>% filter select mutate
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom tidyr expand_grid

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
