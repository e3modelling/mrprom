#' convertEurostat2
#'
#' @param x MAgPIE object.
#'
#' @return The "EUROSTAT" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("Eurostat2", convert = TRUE)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr select
#'

convertEurostat2 <- function(x) {

  x <- as.quitte(x)
  region <- NULL
  x <- select(x, -c(region))
  names(x) <- sub("geo","region", names(x))

  x[["region"]] <- toolCountry2isocode(x[["region"]],
                                       mapping = c("EL" = "GRC",
                                                   "XK" = "KOS"))


  x <- x[!is.na(x$region), ]
  x <- x[!is.na(x$value), ]
  x <- as.quitte(x)
  x <- as.magpie(x)
  x <- toolCountryFill(x, fill = 0)

  return(x[as.character(getISOlist()), , ])
}
