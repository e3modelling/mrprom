#' convertFAOForestry
#'
#' The ISO codes of "FAOForestry" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "FAOForestry" data with spatial entries for each country.
#'
#' @author Fotis Sioutas
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("FAOForestry", convert = TRUE)
#' }
#' 
#' @importFrom dplyr filter %>%
#' @importFrom quitte as.quitte
#'
convertFAOForestry <- function(x) {
  x <- as.quitte(x)
  x <- x %>%
    dplyr::filter(
      !is.na(region),
      !region %in% c(
        "Polynesia",
        "Micronesia (Federated States of)",
        "China"
      )
    )
  
  suppressWarnings({
    levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]),
                                                 mapping =
                                                   c(
                                                     "Netherlands (Kingdom of the)" = "NLD",
                                                     "Ascension, Saint Helena and Tristan da Cunha" = "SHN",
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
