#' convertUNFCCC
#'
#' The ISO codes of "UNFCCC" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "UNFCCC" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- readSource("UNFCCC", subtype = "2.  Industrial Processes and Product Use", convert = TRUE)
#' }
#'

convertUNFCCC <- function(x) {
  
  x <- toolCountryFill(x, fill = NA)
  return(x[as.character(getISOlist()), , ])
  
}
