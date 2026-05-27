#' convertEurostatHDD
#'
#' The ISO codes of "EurostatHDD" data are compared with the official ISO code country list.
#' NA values are replaced with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "EurostatHDD" data with spatial entries for each country.
#'
#' @author Fotis Sioutas
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("EurostatHDD", convert = TRUE)
#' }
#'
convertEurostatHDD <- function(x) {
  
  x <- as.quitte(x)
  
  suppressWarnings({
    levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]))
  })
  
  x <- filter(x, !is.na(x[["region"]]))
  x <- as.magpie(x)
  suppressMessages(
    suppressWarnings(
      x <- toolCountryFill(x, fill = NA)
    )
  )
  return(x[as.character(getISOlist()), , ])
}
