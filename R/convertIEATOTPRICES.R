#' convertIEATOTPRICES
#'
#' The ISO codes of "IEATOTPRICES" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "IEAEnergyPrices" data with spatial entries for each country.
#'
#' @author Fotis Sioutas
#' 
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEATOTPRICES", convert = TRUE)
#' }
#'

convertIEATOTPRICES <- function(x) {
  
  x <- toolCountryFill(x, fill = NA)
  
  return(x[as.character(getISOlist()), , ])
  
}
