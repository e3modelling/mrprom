#' convertENERDATA
#'
#' The two available ENERDATA datasets are combined into one,
#' NA values are replaced with zeros and historical data are corrected.
#'
#' @param x MAgPIE object with ISO country codes.
#'
#' @param subtype string. By choosing a subtype you filter the ENERDATA dataset
#' (1800+ variables) by type, to allow further processing of specific variables
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
#' a <- readSource("ENERDATA", subtype =  "electricity production", convert = TRUE)
#' }
#'

convertENERDATA <- function(x, subtype) {

  x2 <- readSource("ENERDATA2", subtype, convert = FALSE)
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

  x <- toolCountryFill(x, fill = NA)
  
  return(x[as.character(getISOlist()), , ])
}
