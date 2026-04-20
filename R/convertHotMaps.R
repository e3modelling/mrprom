#' convertEurostatHDD
#'
#' The ISO codes of "EurostatHDD" data are compared with the official ISO code country list.
#' NA values are replaced with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "EurostatHDD" data with spatial entries for each country.
#'
#' @author Margarita Efthymiadou
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("HotMaps", convert = TRUE)
#' }
#'
convertHotMaps <- function(x) {
  
  x <- as.quitte(x)

  # FIX: adjust levels BEFORE mapping
  levs <- levels(x[["region"]])

  levs[levs == "EL"] <- "GR"
  levs[levs == "UK"] <- "GB"

  suppressWarnings({
    levels(x[["region"]]) <- toolCountry2isocode(levs)
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
