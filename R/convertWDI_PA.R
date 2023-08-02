#' convertWDI_PA
#'
#' The ISO codes of "WDI_PA" data are compared with the official ISO
#' code country list by using the function toolCountryFill of madrat.
#'
#' @param x MAgPIE object with ISO country codes.
#'
#' @return The "WDI_PA" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("WDI_PA", convert = TRUE)
#' }
#'
#' @importFrom quitte as.quitte

convertWDI_PA <- function(x) {

  x <- toolCountryFill(x, fill = 0)
  return(x[as.character(getISOlist()), , ])
}
