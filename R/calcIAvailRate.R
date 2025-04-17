#' calcIAvailRate
#'
#' Use data from EU Reference Scenario to derive OPENPROM input parameter iAvailRate
#' This dataset includes plant availability rate, as a percentage.
#' 
#' @return magpie object with OPENPROM input data iAvailRate 
#' 
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IAvailRate", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select filter rename mutate case_when
#' @importFrom tidyr pivot_wider spread gather
#' @importFrom quitte as.quitte interpolate_missing_periods
 
calcIAvailRate <- function() {

  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fEndHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndHorizon"]

  # Get power plant set from GAMS code
  sets <- toolGetMapping(name = "PGALL.csv",
                         type = "blabla_export",
                         where = "mrprom")
  
  set_pgall <- as.character(sets[, 1])

  df <- data.frame(variable = set_pgall,
                   period = 2020,
                   unit = "Percentage",
                   value = 0.0)

  # FIXME: The plant availability rates are missing from EU Reference Scenario 2020
  # Temporarily adding data from E3M_PRIMES_tech_assumptions_version_Oct2019_fv.xlsx
  df <- mutate(df, value = case_when(
  variable == "CTHBMSWAS" ~ 0.85, variable == "ATHLGN" ~ 0.85, variable == "ATHHCL" ~ 0.85,
  variable == "ATHRFO" ~ 0.8,     variable == "ATHNGS" ~ 0.8,  variable == "ATHBMSWAS" ~ 0.85, 
  variable == "SUPCRL" ~ 0.85,    variable == "SUPCR" ~ 0.85,  variable == "FBCLGN" ~ 0.85,
  variable == "FBCHCL" ~ 0.85,    variable == "IGCCLGN" ~ 0.85, variable == "IGCCHCL" ~ 0.85,
  variable == "IGCCBMS" ~ 0.85,   variable == "CCCGT" ~ 0.8,   variable == "ACCGT" ~ 0.8,
  variable == "AGTGDO" ~ 0.29,    variable == "AGTNGS" ~ 0.29, variable == "PGLHYD" ~ 0.67,
  variable == "PGSHYD" ~ 0.67,    variable == "PGWND" ~ 0.225, variable == "PGSOL" ~ 0.2,
  variable == "PGASHYD" ~ 0.67,   variable == "PGAWND" ~ 0.29, variable == "PGASOL" ~ 0.25,
  variable == "PGADPV" ~ 0.2,     variable == "PGAOTHREN" ~ 0.45, variable == "PGANUC" ~ 0.9,
  variable == "PGAPSS" ~ 0.85,    variable == "PGAPSSL" ~ 0.85, variable == "PGACGSL" ~ 0.85,
  variable == "PGACGS" ~ 0.85,    variable == "PGAGGS" ~ 0.85,    variable == "PGAWNO" ~ 0.32,
  variable == "ATHBMCCS" ~ 0.85, TRUE ~ value))

  # Interpolating the missing values for the specified time period
  xq <- as.quitte(df)
  xq <- interpolate_missing_periods(xq, seq(fStartHorizon, fEndHorizon, 1), expand.values = TRUE)

  # Converting to magpie object
  x <- as.quitte(xq) %>% as.magpie()
  # Set NA to 0
  x[is.na(x)] <- 0
  list(x = x,
       weight = NULL,
       unit = "Percentage",
       description = "EU Reference Scenario 2020; Plant Availability Rate")
}
