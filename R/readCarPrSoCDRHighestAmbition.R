#' readCarPrSoCDRHighestAmbition
#'
#' Read CarPrSoCDRHighestAmbition.
#' The dataset contains carbon price data.
#'
#' @return The read-in carbon price data into a magpie object
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("CarPrSoCDRHighestAmbition")
#' }
#'
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom utils read.csv
#' @importFrom quitte as.quitte
#'
readCarPrSoCDRHighestAmbition <- function() {

  x <- read.csv(file = "CarPrSoCDRHighestAmbition.csv")

  names(x) <- sub("X", "", names(x))

  x <- x %>%
    pivot_longer(
      cols = `2010`:`2100`,
      names_to = "period",
      values_to = "value"
    )

  x <- as.quitte(x) %>% as.magpie()

  map <- toolGetMapping("regionmappingOPDEV5.csv", "regional", where = "mrprom")

  x <- toolAggregate(x, rel = map, weight = NULL, from = "Region.Code", to = "ISO3.Code", dim = 1)

  list(x = x,
       weight = NULL,
       description = c(category = "Costs",
                       type = "Carbon Price",
                       filename = "CarPrSoCDRHighestAmbition.csv",
                       `Indicative size (MB)` = 0.21,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "project"))

}