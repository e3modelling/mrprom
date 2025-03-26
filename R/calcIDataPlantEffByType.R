#' calcIDataPlantEffByType
#'
#' Use data from IEA and EU Reference Scenario to derive OPENPROM input parameter iDataPlantEffByType
#' This dataset includes plant efficiency per plant type, as a ratio.
#'
#' @return magpie object with OPENPROM input data iDataPlantEffByType.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataPlantEffByType", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select filter rename
#' @importFrom tidyr pivot_wider spread gather complete expand
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIDataPlantEffByType <- function() {

  x <- readSource("TechCosts", "PowerAndHeatEfficiency", convert = TRUE)

  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fEndHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndHorizon"]

  # Use PRIMES - OPENPROM mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-primes-pgall-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")
  xq <- as.quitte(x)
  merged <- merge(map, xq, by.x = "PRIMES", by.y = "variable") # INNER JOIN

  # Renaming and dropping columns
  xq <- select(merged, -c("PRIMES"))
  xq <- rename(xq, "variable" = "OPEN.PROM")

  # FIXME: Some power plant types are missing from EU Reference Scenario 2020
  # Temporarily adding data from PRIMES_COSTS/techn2009.xlsx
  df_missing <- data.frame(
  variable = c("ATHRFO", "ATHRFO", "ATHRFO", "ATHRFO", "AGTGDO", "AGTGDO", "AGTGDO", "AGTGDO"),
  model = rep("(Missing)", 8),
  scenario = rep("(Missing)", 8),
  region = rep("GLO", 8),
  unit = rep("ratio", 8),
  period = c(2020, 2030, 2040, 2050, 2020, 2030, 2040, 2050),
  value = c(0.3912, 0.3926, 0.3952, 0.3978, 0.3846, 0.3875, 0.3930, 0.3984))
  xq <- rbind(xq, df_missing)

  # Interpolating the missing values for the specified time period
  xq <- interpolate_missing_periods(xq, seq(fStartHorizon, fEndHorizon, 1), expand.values = TRUE)
  
  # Use IEA - OPENPROM mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-IEA-pgall-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")

  a <- readSource("IEA_WEO_TechCosts", convert = TRUE)
  
  qa <- as.quitte(a)
  merged <- merge(map, qa, by.x = "IEA", by.y = "technology") # INNER JOIN
  
  scenario <- NULL
  variable <- NULL
  
  merged <- filter(merged, scenario == "Stated Policies")
  merged <- filter(merged, variable == "Efficiency")
  
  # Renaming and dropping columns
  xqIEA <- select(merged, -c("PRIMES", "IEA", "variable"))
  xqIEA <- rename(xqIEA, "variable" = "OPEN.PROM")
  
  # Interpolating the missing values for the specified time period
  xqIEA <- interpolate_missing_periods(xqIEA, seq(fStartHorizon, fEndHorizon, 1), expand.values = TRUE)
  
  qx <- rbind(xq, xqIEA)
  
  region <- NULL
  period <- NULL
  
  qx <- qx %>% complete(variable, nesting(region, period))
  
  # Assign the global from RefScen where necessary
  qx <- filter(qx, region != "GLO")
  
  value.x <- NULL
  value.y <- NULL
  qx <- left_join(qx, xq, by = c("variable", "period")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y", "scenario.y", "region.y", "unit.y", "model.y"))
  names(qx) <- sub("region.x", "region", names(qx))
  names(qx) <- sub("scenario.x", "scenario", names(qx))
  names(qx) <- sub("unit.x", "unit", names(qx))
  names(qx) <- sub("model.x", "model", names(qx))

  # Converting to magpie object
  x <- as.quitte(qx) %>% as.magpie()
  # Set NA to 0
  x[is.na(x)] <- 0
  
  #fix units
  getItems(x,3.3) <- c("gross, LHV", "gross, LHV")
  
  list(x = x,
       weight = NULL,
       unit = "Ratio",
       description = "IEA, EU Reference Scenario 2020; Plant Efficiency")
}
