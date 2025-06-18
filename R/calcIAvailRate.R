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
  Prod <- calcOutput(type = "IDataElecProd", mode = "Total", aggregate = FALSE) %>% as.quitte()
  Cap <- calcOutput(type = "IInstCapPast", mode = "Total", aggregate = FALSE) %>% as.quitte()
  baseYear <- 2020

  availRate <- Prod %>%
    select(c("region", "period", "variable", "value")) %>%
    left_join(Cap, by = c("region", "period", "variable")) %>%
    mutate(
      value.z = value.x / (value.y * 8.76),
      value = ifelse(is.nan(value.z) | is.infinite(value.z), 0, value.z)
    ) %>%
    correctAvailRate() %>%
    filter(period == baseYear) %>%
    select(c("region", "variable", "value")) %>%
    as.quitte() %>%
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
    rate = c(
      0.85, 0.85, 0.8, 0.8, 0.85,
      0.67, 0.67, 0.2, 0.225, 0.2,
      0.45, 0.9, 0.85, 0.85, 0.85,
      0.32, 0.85
    )
  )
  availRate <- availRate %>%
    left_join(lookupTable, by = "variable") %>%
    mutate(value = ifelse(value < 1, value, rate)) %>%
    select(-rate)
  return(availRate)
}
