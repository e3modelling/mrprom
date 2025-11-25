#' convertEMBER
#'
#' The ISO codes of "EMBER" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "EMBER" data with spatial entries for each country.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("EMBER", convert = TRUE)
#' }
#'

convertEMBER <- function(x) {
  
  suppressWarnings({
    x <- toolCountryFill(x, fill = NA)
  })
  
  return(x[as.character(getISOlist()), , ])
  
}
