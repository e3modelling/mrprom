#' convertFAOprojections
#'
#' The ISO codes of "FAO projections" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "FAO" data with spatial entries for each country.
#'
#' @author Fotis Sioutas
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("FAOprojections", convert = TRUE)
#' }
#' 
#' @importFrom dplyr filter %>%
#' @importFrom quitte as.quitte
#'
convertFAOprojections <- function(x) {
  x <- as.quitte(x)
  
  suppressWarnings({
    levels(x[["region"]]) <- toolCountry2isocode(
      levels(x[["region"]]),
      mapping = c(
        "China, mainland" = "CHN"
      )
    )
  })
  x <- x %>%
    dplyr::filter(!is.na(region))
  x <- as.quitte(x)
  x <- as.magpie(x)
  suppressMessages(
    suppressWarnings(
      x <- toolCountryFill(x, fill = NA)
    )
  )
  return(x[as.character(getISOlist()), , ])
}
