#' readOECD_SSP_GDP
#'
#' Read in data from the OECD_SSP_GDP.
#' The dataset contains GDP and population data.
#'
#'
#' @return The read-in data into a magpie object
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("OECD_SSP_GDP", convert = TRUE)
#' }
#' @importFrom dplyr select matches mutate starts_with filter
#' @importFrom dplyr bind_rows left_join if_else
#' @importFrom tidyr pivot_longer
#' @importFrom utils read.csv
#' @importFrom quitte as.quitte
#'
readOECD_SSP_GDP <- function() {
  
  GDP_SSP1 <- read.csv(file = "GDP_SSP1.csv")
  GDP_SSP2 <- read.csv(file = "GDP_SSP2.csv")
  GDP_SSP3 <- read.csv(file = "GDP_SSP3.csv")
  GDP_SSP4 <- read.csv(file = "GDP_SSP4.csv")
  GDP_SSP5 <- read.csv(file = "GDP_SSP5.csv")
  
  # Historical reference from SSP2
  historical <- GDP_SSP2 %>%
    filter(scenario == "Historical Reference") %>%
    select(region, variable, unit, version, starts_with("X")) %>%
    pivot_longer(
      cols = matches("^X[0-9]{4}$"),
      names_to = "period",
      values_to = "historical_value"
    ) %>%
    mutate(
      period = as.integer(sub("^X", "", period))
    ) %>%
    filter(period < 2025)
  
  # Combine all SSP scenarios
  GDP <- bind_rows(
    GDP_SSP1, GDP_SSP2, GDP_SSP3, GDP_SSP4, GDP_SSP5
  ) %>%
    filter(scenario != "Historical Reference") %>%
    pivot_longer(
      cols = matches("^X[0-9]{4}$"),
      names_to = "period",
      values_to = "value"
    ) %>%
    mutate(
      period = as.integer(sub("^X", "", period))
    ) %>%
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
      unit == "billion USD_2015/yr",
      period >= 2010,
      period <= 2100
    )
  
  # Convert to magpie
  x <- as.quitte(GDP) %>%
    as.magpie()
  
  return(list(
    x = x,
    weight = NULL,
    description = c(
      category = "GDP",
      type = "GDP",
      filename = paste(
        "GDP_SSP1.csv",
        "GDP_SSP2.csv",
        "GDP_SSP3.csv",
        "GDP_SSP4.csv",
        "GDP_SSP5.csv",
        sep = "; "
      ),
      `Indicative size (MB)` = 0.27,
      dimensions = "3D",
      unit = "billion USD_2015/yr",
      Confidential = "open"
    )
  ))
}