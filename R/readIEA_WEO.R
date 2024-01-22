#' readIEA_WEO
#'
#' Read fossil fuel price projections from the IEA World Energy Outlook 2023
#' The projections are based on the Stated Policies Scenario (STEPS).
#'
#' @param subtype Type of data that should be read.
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_WEO", subtype = "FuelPrices")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr %>% select mutate rename
#' @importFrom tidyr pivot_longer spread gather
#' @importFrom quitte as.quitte
#'
readIEA_WEO<- function(subtype = "FuelPrices") {
  
  # Read the Excel file
  data <- read_excel("IEA_STEPS_prices.xlsx")
  selected_data <- data[ c(1, 2, 7), ]
  
  # Pivot to a long dataframe
  long_data <- selected_data %>% 
    pivot_longer( cols = -c(1, 6), 
    names_to = "year", values_to = "value") %>%
    rename(variable = 'Real terms (USD 2022)', unit = Unit)
  
  # Convert to magpie object
  x <- as.quitte(long_data) %>% as.magpie()

  # Converting the values from $2022 to $2015
  # assuming a cumulative price change of -19%
  x <- x * 0.81
  
  # Converting the crude oil price from $/Barrel to $/toe
  x[, , "IEA crude oil"] <- x[, , "IEA crude oil"] * 7.2
  
  # Converting the natural gas price from $/MBtu to $/toe
  x[, , "Natural gas average"] <-  x[, , "Natural gas average"] * 39.652
  
  # Converting the coal price from $/tn to $/toe
  x[, , "Steam coal average"] <- x[, , "Steam coal average"] * 1.666
  
  # Setting the new unit for all rows
  xq <- as.quitte(x)
  xq[['unit']] <- "$2015/toe"
  xq[['unit']] <- factor(xq[['unit']])
  
  return(as.magpie(xq))
}
