#' readDAC
#'
#' Read fossil fuel price projections from the IEA World Energy Outlook 2023
#' global source of energy analysis and projections.
#' The projections are based on the Stated Policies Scenario (STEPS).
#' It identifies and explores the biggest trends in energy demand and supply,
#' as well as what they mean for energy security, emissions and
#' economic development
#'
#' @return The read-in data into a magpie object.
#'
#' @author DIONYSIS P
#'
#' @examples
#' \dontrun{
#' a <- readSource("DAC")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr %>% select mutate rename
#' @importFrom tidyr pivot_longer spread gather
#' @importFrom quitte as.quitte
#'
readDAC <- function() {
  
  # Read the Excel file
  data <- read_excel("DAC_input.xlsx",sheet = "DAC input")
  
  
  # Convert to magpie object
  y <- as.quitte(data) %>% as.magpie()
  
  x <- toolCountryFill(y, fill = NA)
  
  x[getISOlist(),2020,] <- y["GLO",,]
  
  list(x = x,
       weight = NULL,
       description = c(category = "Costs",
                       type = "Read fossil fuel price projections",
                       filename = "IEA_STEPS_prices.xlsx",
                       `Indicative size (MB)` = 0.012,
                       dimensions = "2D",
                       unit = "$2015/toe",
                       Confidential = "E3M"))
}
