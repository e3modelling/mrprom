#' calcIDataPlantEffByType
#'
#' Use data from EU Reference Scenario to derive OPENPROM input parameter iDataPlantEffByType
#' This dataset includes plant efficiency per plant type, as a ratio.
#' 
#' @return magpie object with OPENPROM input data iDataPlantEffByType 
#' 
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataPlantEffByType", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select filter rename
#' @importFrom tidyr pivot_wider spread gather
#' @importFrom quitte as.quitte interpolate_missing_periods
 
calcIDataPlantEffByType <- function() {

  x <- readSource("TechCosts", "PowerAndHeatEfficiency", convert = TRUE)
  
  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fEndHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndHorizon"]
  
  # Use PRIMES - OPENPROM mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-primes-pgall-mapping.csv",
                        type = "sectoral",
                        where = "mappingfolder")
  xq <- as.quitte(x)
  merged <- merge(map, xq, by.x = "PRIMES", by.y = "variable") # INNER JOIN
  
  # Renaming and dropping columns
  xq <- select(merged, -c("PRIMES"))
  xq <- rename(xq, "variable" = "OPEN.PROM")
  
  # FIXME: Some power plant types are missing from EU Reference Scenario 2020
  # Temporarily adding data from PRIMES_COSTS/techn2009.xlsx
  df_missing <- data.frame(
  variable = c("ATHRFO", "ATHRFO", "ATHRFO", "ATHRFO", "AGTGDO", "AGTGDO","AGTGDO","AGTGDO"),
  model = rep("(Missing)", 8),
  scenario = rep("(Missing)", 8),
  region = rep("GLO", 8),
  unit = rep("ratio", 8),
  period = c(2020, 2030, 2040, 2050, 2020, 2030, 2040, 2050),
  value = c(0.3912, 0.3926, 0.3952, 0.3978, 0.3846, 0.3875, 0.3930, 0.3984) )
  xq <- rbind(xq, df_missing)
  
  # Interpolating the missing values for the specified time period
  xq <- interpolate_missing_periods(xq, seq(fStartHorizon, fEndHorizon, 1), expand.values = TRUE)
  
  # Converting to magpie object
  x <- as.quitte(xq) %>% as.magpie()
  # Set NA to 0
  x[is.na(x)] <- 0
  list(x = x,
       weight = NULL,
       unit = "Ratio",
       description = "EU Reference Scenario 2020; Plant Efficiency")
}
