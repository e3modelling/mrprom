#' calcIVarCost
#'
#' Use data from EU Reference Scenario to derive OPENPROM input parameter iVarCost
#' This dataset includes variable non fuel cost per plant type, in $2015/MWh.
#'
#' @return magpie object with OPENPROM input data iVarCost.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IVarCost", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select filter rename mutate case_when
#' @importFrom tidyr pivot_wider spread gather
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIVarCost <- function() {

  x <- readSource("TechCosts2024", "PowerAndHeat", convert = TRUE)

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
  # Filtering the variable cost per plant type rows and dropping unnecessary columns
  xq <- filter(merged, variable == "Variable non fuel cost")
  xq <- xq %>% select(-c("PRIMES", "variable")) %>%
               rename("variable" = "OPEN.PROM")
  xq[["unit"]] <- "$2015/MWh"

  # Replacing zero values with 1e-6 to avoid bugs in GAMS
  xq <- xq %>%
    mutate(value = case_when(
      value == 0 ~ 0.000001, TRUE ~ value))

  # FIXME: Some power plant types are missing from EU Reference Scenario 2020
  # Temporarily adding data from E3M_PRIMES_tech_assumptions_version_Oct2019_fv.xlsx
  df_missing <- data.frame(
  variable = c("ATHOIL", "ATHOIL", "ATHOIL", "ATHOIL"),
  model = rep("(Missing)", 4),
  scenario = rep("(Missing)", 4),
  region = rep("GLO", 4),
  unit = rep("$2015/MWh", 4),
  period = c(2020, 2030, 2040, 2050),
  value = c(4.84, 4.84, 4.84, 4.84))
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
       unit = "$2015/MWh",
       description = "EU Reference Scenario 2020; Variable Non Fuel Cost")
}
