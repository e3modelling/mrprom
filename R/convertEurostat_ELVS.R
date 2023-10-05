#' convertEurostat_ELVS
#'
#' The ISO codes of the data are compared with the official ISO
#' code country list by using the function toolCountryFill of madrat.
#'
#' @param x MAgPIE object with ISO country codes.
#'
#' @return The "Eurostat_ELVS" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("Eurostat_ELVS", convert = TRUE)
#' }
#'

convertEurostat_ELVS <- function(x) {
  
    
  x <- toolCountryFill(x, fill = NA)
  return(x[as.character(getISOlist()), , ])
}
