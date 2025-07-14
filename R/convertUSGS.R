#' convertUSGS
#'
#' The ISO codes of "USGS" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "USGS" data with spatial entries for each country.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("USGS", convert = TRUE)
#' }
#'

convertUSGS <- function(x) {
  
  x <- toolCountryFill(x, fill = NA)
  return(x[as.character(getISOlist()), , ])
  
}
