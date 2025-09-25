#' convertEuropeanHydrogenPrices
#'
#' The ISO codes of "EuropeanHydrogenPrices" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "European Hydrogen Observatory" data with spatial entries for each country.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("EuropeanHydrogenPrices", convert = TRUE)
#' }
#'

convertEuropeanHydrogenPrices <- function(x) {
  
  x <- toolCountryFill(x, fill = NA)
  return(x[as.character(getISOlist()), , ])
  
}
