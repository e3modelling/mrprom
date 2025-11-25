#' readEurostat_Transport
#'
#' Read in a csv file and convert it to a magpie object
#' The data has information about transport passengers per country and per year
#'
#' @return magpie object with the requested output data about transport passengers per country and per year
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("Eurostat_Transport")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom utils read.csv
#'

readEurostat_Transport <- function() {

  value <- NULL
  x <- read.csv("sum_pkm.csv")
  x <- x[23:57, c(2, 4)]
  names(x) <- sub("Passengers..tonnes", "region", names(x))
  names(x) <- sub("X.2", "value", names(x))
  x["value"] <- as.numeric(gsub(",", "", x$value))
  x["unit"] <- "pkm"
  x["period"] <- "2017"
  
  suppressWarnings({
    x[, "region"] <- toolCountry2isocode((x[, "region"]),
                                         mapping = c("EL" = "GRC"))
  })

  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "Transport",
                       type = "Transport Passengers",
                       filename = "sum_pkm.csv",
                       `Indicative size (MB)` = 0.005,
                       dimensions = "2D",
                       unit = "pkm",
                       Confidential = "open"))

}
