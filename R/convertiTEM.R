#' convertiTEM
#'
#' The ISO codes of "iTEM" data are compared with the official ISO code country list.
#' NA values are replaced with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "iTEM" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- readSource("iTEM", convert = TRUE)
#' }
#'

convertiTEM <- function(x) {
  x <- toolCountryFill(x, fill = NA)
  return(x[as.character(getISOlist()), , ])
}
