#' convertUN
#'
#' The ISO codes of "UN" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "UN" data with spatial entries for each country.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("UN", convert = TRUE)
#' }
#'

convertUN <- function(x) {
  
  x <- toolCountryFill(x, fill = NA)
  return(x[as.character(getISOlist()), , ])
  
}
