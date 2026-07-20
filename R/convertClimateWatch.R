#' convertClimateWatch
#'
#' The ISO codes of "ClimateWatch" data are compared with the official ISO code country list.
#' NA values are replaced with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "ClimateWatch" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Alexandros Tsimpoukis
#'
#' @examples
#' \dontrun{
#' a <- readSource("ClimateWatch", convert = TRUE)
#' }
#'

convertClimateWatch <- function(x) {
  
  suppressMessages(
    suppressWarnings(
      x <- toolCountryFill(x, fill = NA)
    )
  )
  
  return(x[as.character(getISOlist()), , ])
  
}
