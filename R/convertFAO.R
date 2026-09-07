#' convertFAO
#'
#' The ISO codes of "FAO" data are compared with the official ISO code country list.
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
#' a <- readSource("FAO", convert = TRUE)
#' }
#' 
#' @importFrom dplyr filter %>%
#' @importFrom magrittr 
#' @importFrom quitte as.quitte
#'
convertFAO <- function(x) {
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
                                                     "Netherlands (Kingdom of the)" = "NLD"
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
