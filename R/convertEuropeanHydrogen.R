#' convertEuropeanHydrogen
#'
#' The ISO codes of "EuropeanHydrogen" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "European Hydrogen Observatory" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("EuropeanHydrogen", convert = TRUE)
#' }
#'

convertEuropeanHydrogen <- function(x) {
  
  suppressMessages(
    suppressWarnings(
      x <- toolCountryFill(x, fill = NA)
    )
  )

  return(x[as.character(getISOlist()), , ])
  
}
