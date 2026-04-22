#' convertEURefPlus
#'
#' The ISO codes of "EURefPlus" data are compared with the official ISO code country list.
#' NA values are replaced with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "EURefPlus" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @importFrom quitte as.quitte
#'
#' @examples
#' \dontrun{
#' a <- readSource("EURefPlus", convert = TRUE)
#' }
#'
convertEURefPlus <- function(x) {
  x <- as.quitte(x)
  
  suppressWarnings({
    levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]),
                                                 mapping =
                                                   c(
                                                     "EL" = "GRC",
                                                     "EU27" = "EU27"
                                                   )
    )
  })

  x <- as.magpie(x)
  suppressMessages(
    suppressWarnings(
      x <- toolCountryFill(x, fill = NA)
    )
  )
  return(x[as.character(getISOlist()), , ])
}
