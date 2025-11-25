#' convertGidden
#'
#' The ISO codes of "Gidden" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "Gidden" data with spatial entries for each country.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("Gidden", convert = TRUE)
#' }
#'
#' @importFrom dplyr "%>%"

convertGidden <- function(x) {
  
  suppressWarnings({
    x <- toolCountryFill(x, fill = NA)
  })
  return(x[as.character(getISOlist()), , ])
  
}
