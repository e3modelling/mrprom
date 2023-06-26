#' convertENERDATA
#'
#' NA values are replaced with zeros and historical data are corrected.
#'
#' @param x MAgPIE object with ISO country codes.
#'
#' @return The "ENERDATA" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' #' @examples
#' \dontrun{
#' a <- readSource("ENERDATA", convert = TRUE)
#' }
#'

convertENERDATA <- function(x) {


  x <- toolCountryFill(x, fill = 0)
  x <- toolISOhistorical(x)
  return(x[as.character(getISOlist()), , ])
}
