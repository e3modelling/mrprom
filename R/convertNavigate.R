#' convertNavigate
#'
#' The ISO codes of "Navigate" data are compared with the official ISO code country list.
#' NA values are replaced with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "Navigate" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = TRUE)
#' }
#'

convertNavigate <- function(x) {
  
  x <- toolCountryFill(x, fill = NA) 
  return(x[as.character(getISOlist()), , ])
  
}
