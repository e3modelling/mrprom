#' convertEDGAR2026
#'
#' The ISO codes of "EDGAR2026" data are compared with the official ISO code country list.
#' NA values are replaced with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "EDGAR2026" data with spatial entries for each country.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("EDGAR2026", convert = TRUE)
#' }
#'

convertEDGAR2026 <- function(x) {
  
  suppressMessages(
    suppressWarnings(
      x <- toolCountryFill(x, fill = NA)
    )
  )
  
  return(x[as.character(getISOlist()), , ])
  
}
