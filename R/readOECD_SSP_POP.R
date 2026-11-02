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
#'
#' @importFrom dplyr select matches mutate across rename_with starts_with filter
#' @importFrom tidyr pivot_longer
#' @importFrom utils read.csv
#' @importFrom quitte as.quitte
#'
readOECD_SSP_POP <- function() {
  
  POP_SSP1 <- read.csv(file = "Population_SSP1.csv")
  POP_SSP2 <- read.csv(file = "Population_SSP2.csv")
  POP_SSP3 <- read.csv(file = "Population_SSP3.csv")
  POP_SSP4 <- read.csv(file = "Population_SSP4.csv")
  POP_SSP5 <- read.csv(file = "Population_SSP5.csv")
  
  # Historical reference from SSP2
  historical <- POP_SSP2 %>%
    filter(scenario == "Historical Reference") %>%
    select(region, variable, unit, version, starts_with("X"))
  
  POP <- bind_rows(POP_SSP1, POP_SSP2, POP_SSP3, POP_SSP4, POP_SSP5) %>%
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
  
  POP <- POP %>%
    pivot_longer(
      cols = matches("^[0-9]{4}$"),
      names_to = "period",
      values_to = "value"
    ) %>%
    filter(!is.na(value)) %>%
    select(-c(model, version))
  
  x <- as.quitte(POP) %>% as.magpie()
  
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
