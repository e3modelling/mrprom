#' convertFAOLandUse
#'
#' The ISO codes of "FAOLandUse" data are compared with the official ISO code country list.
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
#' a <- readSource("FAOLandUse", convert = TRUE)
#' }
#' 
#' @importFrom dplyr filter %>%
#' @importFrom quitte as.quitte
#'
convertFAOLandUse <- function(x) {
  x <- as.quitte(x)
  x <- x %>%
    dplyr::filter(
      !is.na(region),
      !region %in% c(
        "Polynesia",
        "Micronesia (Federated States of)"
      )
    )
  
  suppressWarnings({
    levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]),
                                                 mapping =
                                                   c(
                                                     "Netherlands (Kingdom of the)" = "NLD",
                                                     "Ascension, Saint Helena and Tristan da Cunha" = "SHN"
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
