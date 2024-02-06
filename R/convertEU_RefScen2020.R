#' convertEU_RefScen2020
#'
#' Read in data from the EU_RefScen2020.
#' The dataset contains carbon price data for the EU Reference Scenario 2020.
#' This function uses toolCountryFill to fill missing country data with 0 and
#' assign to all EU28 countries the value of EUR.
#'
#' @param x MAgPIE object.
#'
#' @return The read-in carbon price data into a magpie object
#'
#' @author Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- readSource("EU_RefScen2020", convert = TRUE)
#' }
#'
#'
convertEU_RefScen2020 <- function(x) {

  map <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  eu <- toolAggregate(x["EUR", , ], rel = map, partrel = TRUE)
  x <- toolCountryFill(x["EUR", , , invert = TRUE], fill = 0)

  return(mbind(eu, x[map[map[["RegionCode"]] == "EUR", "CountryCode"], , invert = TRUE]))

}
