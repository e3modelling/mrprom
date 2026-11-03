#' readOECD_SSP_POP
#'
#' Read in data from the OECD_SSP_POP
#' The dataset contain population data.
#'
#' @return The read-in data into a magpie object
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("OECD_SSP_POP", convert = TRUE)
#' }
#' @importFrom dplyr select matches mutate rename_with starts_with
#' @importFrom dplyr filter bind_rows left_join coalesce
#' @importFrom tidyr pivot_longer
#' @importFrom utils read.csv
#' @importFrom quitte as.quitte
#'
readOECD_SSP_POP <- function() {
  
  POP_SSP1 <- read.csv("Population_SSP1.csv")
  POP_SSP2 <- read.csv("Population_SSP2.csv")
  POP_SSP3 <- read.csv("Population_SSP3.csv")
  POP_SSP4 <- read.csv("Population_SSP4.csv")
  POP_SSP5 <- read.csv("Population_SSP5.csv")
  
  # Historical reference from SSP2
  historical <- POP_SSP2 %>%
    filter(scenario == "Historical Reference") %>%
    select(region, variable, unit, version, starts_with("X")) %>%
    pivot_longer(
      cols = matches("^X[0-9]{4}$"),
      names_to = "period",
      values_to = "historical_value"
    ) %>%
    mutate(period = as.integer(sub("^X", "", period))) %>%
    filter(period < 2025)
  
  # Combine SSP scenarios
  POP <- bind_rows(
    POP_SSP1, POP_SSP2, POP_SSP3, POP_SSP4, POP_SSP5
  ) %>%
    filter(scenario != "Historical Reference") %>%
    pivot_longer(
      cols = matches("^X[0-9]{4}$"),
      names_to = "period",
      values_to = "value"
    ) %>%
    mutate(period = as.integer(sub("^X", "", period))) %>%
    left_join(
      historical,
      by = c("region", "variable", "unit", "version", "period")
    ) %>%
    mutate(
      value = if_else(
        period < 2025,
        historical_value,
        value
      )
    ) %>%
    select(-historical_value, -model, -version) %>%
    filter(
      !is.na(value),
      period >= 2010,
      period <= 2100
    )
  
  # Convert to magpie
  x <- as.quitte(POP) %>%
    as.magpie()
  
  return(list(
    x = x,
    weight = NULL,
    description = c(
      category = "Population",
      type = "Population",
      filename = "Population_SSP1.csv; Population_SSP2.csv; Population_SSP3.csv; Population_SSP4.csv; Population_SSP5.csv",
      `Indicative size (MB)` = 0.27,
      dimensions = "3D",
      unit = "various",
      Confidential = "open"
    )
  ))
}