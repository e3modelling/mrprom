#' convertENERDATA
#'
#' The two available ENERDATA datasets are combined into one,
#' NA values are replaced with zeros and historical data are corrected.
#'
#' @param x MAgPIE object with ISO country codes.
#'
#' @return The "ENERDATA" data (filtered by subtype).
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("ENERDATA", convert = TRUE)
#' }
#'

convertENERDATA <- function(x) {

  x2 <- readSource("ENERDATA2", convert = FALSE)
  x2 <- as.quitte(x2)
  x2 <- filter(x2, !is.na(x2[["value"]]))
  levels(x2[["region"]]) <- toolCountry2isocode(levels(x2[["region"]]),
                                       mapping = c("NA" = "NAM",
                                                   "XZ" = "KOS",
                                                   "AN" = "ANT"))
  x2 <- filter(x2, !is.na(x2[["region"]]))

  x <- as.quitte(x)
  levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]),
                                       mapping = c("NA" = "NAM",
                                                   "XZ" = "KOS",
                                                   "AN" = "ANT"))
  x <- filter(x, !is.na(x[["region"]]))
  x <- filter(x, !is.na(x[["value"]]))

  x <-  as.magpie(rbind(x, x2))

  x <- toolCountryFill(x, fill = 0)
  x <- toolISOhistorical(x)
  return(x[as.character(getISOlist()), , ])
}
