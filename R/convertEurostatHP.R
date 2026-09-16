#' convertEurostatHP
#'
#' The ISO codes of "EurostatHP" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "EurostatHP" data with spatial entries for each country.
#'
#' @author Fotis Sioutas
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("EurostatHP", convert = TRUE)
#' }
#'
convertEurostatHP <- function(x) {
  
  x <- as.quitte(x)
  
  suppressWarnings({
    levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]),
                                                mapping = c("EL" = "GRC",
                                                            "XK" = "KOS"))
  })
  
  x <- filter(x, !is.na(x[["region"]]))
  x <- as.magpie(x)
  x[is.na(x)] <- 0
  suppressMessages(
    suppressWarnings(
      x <- toolCountryFill(x, fill = NA)
    )
  )
  return(x[as.character(getISOlist()), , ])
}
