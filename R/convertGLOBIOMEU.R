#' convertGLOBIOMEU
#'
#' The ISO codes of "GLOBIOMEU" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "GLOBIOMEU" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("GLOBIOMEU", convert = TRUE)
#' }
#'

convertGLOBIOMEU <- function(x) {
  
  x <- toolCountryFill(x, fill = NA)
  return(x[as.character(getISOlist()), , ])
  
}
