' readGlobalSolarAtlas
#'
#' Read GlobalSolarAtlas data about solar potential per country.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- readSource("GlobalSolarAtlas", convert = TRUE)
#' }
#'
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr filter select
#' @importFrom readxl read_excel
#'

readGlobalSolarAtlas <- function() {
  
  x <- read_excel("solargis_pvpotential_countryranking_2020_data.xlsx",
                  sheet = "Country indicators")
  
  names(x) <- x[1,]
  x <- x[- 1,]
  x <- select(x, c("ISO_A3", "Evaluated area", "Average theoretical potential (GHI, kWh/m2/day), \r\nlong-term"))
  
  x[["value"]] <- as.numeric(x[["Evaluated area"]]) *
    as.numeric(x[["Average theoretical potential (GHI, kWh/m2/day), \r\nlong-term"]]) /
    8760 * #to GW and to to km2
    365 * #per year
    0.03 #3% of the available non-artificial areas
  
  x[["unit"]] <- "GW"
  names(x)[names(x) == "ISO_A3"] <- "region"
  x[["variable"]] <- "solar"
  x[["period"]] <- 2010
  x <- select(x, c("variable", "region", "unit", "value", "period"))
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "solar potential",
                       type = "solar potential",
                       filename = "solargis_pvpotential_countryranking_2020_data.xlsx",
                       `Indicative size (MB)` = 133,
                       dimensions = "3D",
                       unit = "GW",
                       Confidential = "E3M"))
}