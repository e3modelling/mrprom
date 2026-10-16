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
  
  
  GDP <- read.csv(file = "GDP_SSP2.csv")
  
  GDP <- GDP %>%
    select(
      model, scenario, region, variable, unit, version,
      matches("^X(20[1-9][0-9]|2100)$")
    ) %>%
    mutate(
      across(
        matches("^X20(2[5-9]|[3-9][0-9])$|^X2100$"),
        ~ replace(.x, scenario == "Historical Reference", NA_real_)
      )
    ) %>%
    rename_with(
      ~ sub("^X", "", .x),
      starts_with("X")
    )
  
  GDP <- GDP %>%
    pivot_longer(
      cols = matches("^[0-9]{4}$"),
      names_to = "period",
      values_to = "value"
    ) %>%
    filter(!is.na(value)) %>%
    select(-c(scenario,version)) %>%
    filter(unit == "billion USD_2015/yr")
  
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
