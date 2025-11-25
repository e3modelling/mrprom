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
  
  if (nrow(x2) == 0) {x2 <-  NULL}
  
  x2 <- as.quitte(x2)
  x2 <- filter(x2, !is.na(x2[["value"]]))
  
  suppressWarnings({
    levels(x2[["region"]]) <- toolCountry2isocode(levels(x2[["region"]]),
                                                  mapping = c("NA" = "NAM",
                                                              "XZ" = "KOS",
                                                              "AN" = "ANT"))
  })
  
  
  x2 <- filter(x2, !is.na(x2[["region"]]))
  
  x <- as.quitte(x)
  
  suppressWarnings({
    levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]),
                                                 mapping = c("NA" = "NAM",
                                                             "XZ" = "KOS",
                                                             "AN" = "ANT"))
  })
  
  x <- filter(x, !is.na(x[["region"]]))
  x <- filter(x, !is.na(x[["value"]]))
  
  x <- as.magpie(x)
  x2 <- as.magpie(x2)
  
  Itemsx2 <- setdiff(getItems(x2,3), getItems(x,3))
  
  x2 <- x2[,,Itemsx2]
  
  suppressWarnings({
    x <- toolCountryFill(x, fill = 0)
    x <- toolISOhistorical(x)
    x2 <- toolCountryFill(x2, fill = 0)
    x2 <- toolISOhistorical(x2)
  })
  
  if (is.null(getItems(x2, 3))) {x2 <-  NULL}
  
  x <-  mbind(x, x2)
  
  return(x[as.character(getISOlist()), , ])
}