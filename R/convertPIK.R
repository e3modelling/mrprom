#' convertPIK
#'
#' The ISO codes of "PIK" data are compared with the official ISO code country list.
#' NA values are replaced with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "PIK" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("PIK", convert = TRUE)
#' }
#'

convertPIK <- function(x) {
  
  x <- toolCountryFill(x, fill = NA)
  return(x[as.character(getISOlist()), , ])
  
}
