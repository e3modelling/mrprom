#' convertIEA_CO2
#'
#' The ISO codes of "IEA_CO2" data are compared with the official ISO code country list.
#' NA values are replaced with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "IEA_CO2" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_CO2", convert = TRUE)
#' }
#'

convertIEA_CO2 <- function(x) {
  
  x <- suppressMessages(
    suppressWarnings(
      toolCountryFill(x, fill = NA)
    )
  )
  return(x[as.character(getISOlist()), , ])
  
}
