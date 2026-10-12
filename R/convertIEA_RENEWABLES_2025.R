#' convertIEA_RENEWABLES_2025
#'
#' The ISO codes of "IEA_RENEWABLES_2025" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "IEA_RENEWABLES_2025" data with spatial entries for each country.
#'
#' @author Fotis Sioutas
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_RENEWABLES_2025", convert = TRUE)
#' }
#' 
#' @importFrom dplyr filter %>%
#' @importFrom quitte as.quitte
#'
convertIEA_RENEWABLES_2025 <- function(x) {
  
  x <- as.quitte(x)

  suppressWarnings({
    levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]),
                                                 mapping =
                                                   c(
                                                     "Netherlands (Kingdom of the)" = "NLD",
                                                     "China, mainland" = "CHN",
                                                     "JPN" = "JPN",
                                                     "RUS" = "RUS"
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
