#' convertIMF
#'
#' The ISO codes of "GDP IMF" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "IMF" data with spatial entries for each country.
#'
#' @author Fotis Sioutas
#' 
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("IMF", convert = TRUE)
#' }
#'

convertIMF <- function(x) {
  
  x <- toolCountryFill(x, fill = NA)
  return(x[as.character(getISOlist()), , ])
  
}
