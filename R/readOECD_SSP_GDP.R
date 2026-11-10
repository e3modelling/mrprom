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
  
  # Apply the original processing to each SSP file
  processGDP <- function(GDP, ssp) {
    
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
      filter(
        !is.na(value),
        unit == "billion USD_2015/yr"
      ) %>%
      select(-c(scenario, version)) %>%
      mutate(scenario = ssp)
    
    return(GDP)
  }
  
  # Combine all five SSP scenarios
  GDP <- bind_rows(
    processGDP(GDP_SSP1, "SSP1"),
    processGDP(GDP_SSP2, "SSP2"),
    processGDP(GDP_SSP3, "SSP3"),
    processGDP(GDP_SSP4, "SSP4"),
    processGDP(GDP_SSP5, "SSP5")
  )
  
  GDP <- GDP %>%
    mutate(model = replace(model, model == "Extension", "OECD ENV-Growth 2025"))
  
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