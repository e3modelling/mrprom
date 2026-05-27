#' convertEDGAR2
#'
#' The ISO codes of "EDGAR2" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "EDGAR2" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("EDGAR2", convert = TRUE)
#' }
#'

convertEDGAR2 <- function(x) {
  
  suppressMessages(
    suppressWarnings(
      x <- toolCountryFill(x, fill = NA)
    )
  )
  
  return(x[as.character(getISOlist()), , ])
  
}
