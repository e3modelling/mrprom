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
  
  # Apply the original processing to each SSP file
  processPOP <- function(POP, ssp) {
    
    POP <- POP %>%
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
    
    POP <- POP %>%
      pivot_longer(
        cols = matches("^[0-9]{4}$"),
        names_to = "period",
        values_to = "value"
      ) %>%
      filter(!is.na(value)) %>%
      select(-c(scenario, version)) %>%
      mutate(scenario = ssp)
    
    return(POP)
  }
  
  # Combine all five SSP scenarios
  POP <- bind_rows(
    processPOP(POP_SSP1, "SSP1"),
    processPOP(POP_SSP2, "SSP2"),
    processPOP(POP_SSP3, "SSP3"),
    processPOP(POP_SSP4, "SSP4"),
    processPOP(POP_SSP5, "SSP5")
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