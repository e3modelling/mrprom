#' calcIFixOandMCost
#'
#' Use data from EU Reference Scenario to derive OPENPROM input parameter iFixOandMCost
#' This dataset includes Fixed O&M costs per plant type, in $2015/kW.
#'
#' @return magpie object with OPENPROM input data iFixOandMCost.
#'
#' @author Anastasis Giannousakis, Giannis Tolios, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IFixOandMCost", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select filter rename
#' @importFrom tidyr pivot_wider spread gather
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIFixOandMCost <- function() {

  x <- readSource("TechCosts", "PowerAndHeat", convert = TRUE)

  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fEndHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndHorizon"]

  # Use PRIMES - OPENPROM mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-primes-pgall-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")
  xq <- as.quitte(x)
  merged <- merge(map, xq, by.x = "PRIMES", by.y = "technology") # INNER JOIN

  variable <- NULL
  # Filtering the variable and dropping unnecessary columns
  xq <- filter(merged, variable == "Fixed Operation and Maintenance costs, annually")
  xq <- xq %>% select(-c("PRIMES", "variable")) %>%
    rename("variable" = "OPEN.PROM")

  # FIXME: Some power plant types are missing from EU Reference Scenario 2020
  # Temporarily adding data from E3M_PRIMES_tech_assumptions_version_Oct2019_fv.xlsx
  df_missing <- data.frame(
    variable = c("ATHRFO", "ATHRFO", "ATHRFO", "ATHRFO", "AGTGDO", "AGTGDO", "AGTGDO", "AGTGDO"),
    model = rep("(Missing)", 8),
    scenario = rep("(Missing)", 8),
    region = rep("GLO", 8),
    unit = rep("$2015/kW", 8),
    period = c(2020, 2030, 2040, 2050, 2020, 2030, 2040, 2050),
    value = c(20.7, 20.7, 20.7, 20.7, 13.8, 13.8, 13.8, 13.8))
  xq <- rbind(xq, df_missing)

  # Interpolating the missing values for the specified time period
  xq <- interpolate_missing_periods(xq, seq(fStartHorizon, fEndHorizon, 1), expand.values = TRUE)

  # Converting to magpie object
  x <- as.quitte(xq) %>% as.magpie()
  
  # Converting EUR2015 to $2015
  x <- x * 1.1
  
  # Set NA to 0
  x[is.na(x)] <- 0
  list(x = x,
       weight = NULL,
       unit = "$2015/kW",
       description = "EU Reference Scenario 2020; Fixed O&M costs")
}
