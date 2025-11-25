#' convertEU_COM_RES
#'
#' The ISO codes of "EU_COM_RES" data are compared with the official ISO
#' code country list by using the function toolCountryFill of madrat.
#'
#' @param x MAgPIE object with ISO country codes.
#'
#' @return The "EU_COM_RES" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("EU_COM_RES", convert = TRUE)
#' }
#'


convertEU_COM_RES <- function(x) {

  suppressWarnings({
    x <- toolCountryFill(x, fill = NA)
  })

  return(x[as.character(getISOlist()), , ])
}
