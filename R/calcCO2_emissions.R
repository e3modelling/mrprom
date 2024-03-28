#' calcCO2_emissions
#'
#' Derive CO2_emissions data based on data sources:
#' IEA_CO2 for period 2021
#' EDGAR for period 1970:2022
#' The final CO2 emissions is calculated by finding the share between
#' energy combustion and industrial process emissions from IEA_CO2 and multiply
#' the share with EDGAR data.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "CO2_emissions", aggregate = TRUE)
#' }
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr filter select

calcCO2_emissions <- function() {
  
  a <- readSource("IEA_CO2", convert = TRUE)
  b <- readSource("EDGAR", convert = TRUE)
  
  d <- a[,,"Total CO2 emissions \nfrom fuel combustion.MtCO2"]
  y <- b[,2021,]
  getItems(y, 3) <- NULL
  getItems(d, 3) <- NULL
  share <- d / y
  getItems(share, 2) <- NULL
  x <- b * share
  getItems(x, 3) <- NULL
  
  list(x = x,
       weight = NULL,
       unit = "MtCO2",
       description = "CO2_emissions data")
}
