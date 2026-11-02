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
#'
#' @importFrom dplyr select matches mutate across rename_with starts_with filter
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
    select(region, variable, unit, version, starts_with("X"))
  
  GDP <- bind_rows(GDP_SSP1, GDP_SSP2, GDP_SSP3, GDP_SSP4, GDP_SSP5) %>%
    filter(scenario != "Historical Reference") %>%
    left_join(
      historical,
      by = c("region", "variable", "unit", "version"),
      suffix = c("", "_hist")
    ) %>%
    mutate(
      across(
        matches("^X[0-9]{4}$"),
        ~ coalesce(.x, get(paste0(cur_column(), "_hist")))
      )
    ) %>%
    select(-ends_with("_hist")) %>%
    select(
      model, scenario, region, variable, unit, version,
      matches("^X(20[1-9][0-9]|2100)$")
    ) %>%
    rename_with(~ sub("^X", "", .x), starts_with("X"))
  
  GDP <- GDP %>%
    pivot_longer(
      cols = matches("^[0-9]{4}$"),
      names_to = "period",
      values_to = "value"
    ) %>%
    filter(!is.na(value)) %>%
    select(-c(version)) %>%
    filter(unit == "billion USD_2015/yr")
  
  GDP <- select(GDP,-model)
  
  x <- as.quitte(GDP) %>% as.magpie()
  
  list(x = x,
       weight = NULL,
       description = c(category = "GDP",
                       type = "GDP",
                       filename = "GDP_SSP2.csv",
                       `Indicative size (MB)` = 0.27,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "open"))
  
}
