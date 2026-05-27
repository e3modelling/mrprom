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
#' @importFrom dplyr %>% select filter rename mutate
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIAvailRate <- function() {
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fEndHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndHorizon"]
  Prod <- calcOutput(type = "IDataElecProd", mode = "Total", aggregate = FALSE) %>% as.quitte()
  Cap <- calcOutput(type = "IInstCapPast", mode = "Total", aggregate = FALSE) %>% as.quitte()

  availRate <- Prod %>%
    filter(period < 2024) %>%
    select(c("region", "period", "variable", "value")) %>%
    left_join(Cap, by = c("region", "period", "variable")) %>%
    mutate(
      value.z = value.x / (value.y * 8.76),
      value = ifelse(is.nan(value.z) | is.infinite(value.z), 0, value.z)
    ) %>%
    select(c("region", "period", "variable", "value")) %>%
    correctAvailRate() %>%
    as.quitte() %>%
    interpolate_missing_periods(seq(fStartHorizon, fEndHorizon, 1), expand.values = TRUE) %>%
    as.magpie()

  list(
    x = availRate,
    weight = NULL,
    unit = "Percentage",
    description = "Plant Availability Rate"
  )
}

# Helper---------------------------------------------------------
correctAvailRate <- function(availRate) {
  lookupTable <- data.frame(
    variable = c(
      "ATHLGN", "ATHCOAL", "ATHOIL", "ATHGAS", "ATHBMSWAS",
      "PGLHYD", "PGSHYD", "PGSOL", "PGAWND", "PGCSP",
      "PGOTHREN", "PGANUC", "ATHCOALCCS", "ATHLGNCCS",
      "ATHGASCCS", "PGAWNO", "ATHBMSCCS"
    ),
    lookup = c(
      0.85, 0.85, 0.8, 0.8, 0.85,
      0.67, 0.67, 0.2, 0.225, 0.2,
      0.45, 0.9, 0.85, 0.85, 0.85,
      0.32, 0.85
    )
  )
  correctedAvailRates <- availRate %>%
    filter(value <1 & value > 0 , period == 2020) %>%
    group_by(variable) %>%
    summarise(mean = mean(value, na.rm = TRUE), .groups = "drop") %>%
    right_join(lookupTable, by = "variable") %>%
    mutate(lookup = ifelse(mean > 1 | mean == 0 | is.na(mean), lookup, mean)) %>%
    select(-mean)

  missing_vars <- setdiff(correctedAvailRates$variable, unique(availRate$variable))
  z <- expand(availRate, nesting(region, period), variable = missing_vars) %>%
    mutate(value = NA)

  availRate <- availRate %>%
    rbind(z) %>%
    right_join(correctedAvailRates, by = "variable") %>%
    mutate(value = ifelse(value > 1 | value == 0 | is.na(value), lookup, value)) %>%
    select(-lookup)
  return(availRate)
}
