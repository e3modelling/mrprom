#' convertGEME3
#'
#' The ISO codes of "GEME3" data are compared with the official ISO
#' code country list by using the function toolCountryFill of madrat.
#'
#' @param x MAgPIE object with ISO country codes.
#'
#' @return The "GEME3" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("GEME3", convert = TRUE)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @import mrdrivers



convertGEME3 <- function(x) {

  # disaggregate data to all ISO3 countries based on their GDP or GDP per capita
  GDP <- calcOutput("iGDP", aggregate = FALSE) # will use GDP as disaggregation weights
  GDP <- GDP[, getYears(x), , drop = TRUE]

  Population <- calcOutput("POP", aggregate = FALSE)
  Population <- Population[, getYears(x), , drop = TRUE]

  GDPpCapita <- GDP / Population # will use GDP per capita for disaggregation weights
  GDPpCapita[is.na(GDPpCapita)] <- 0
  ISO3.Code <- NULL
  mapping <- toolGetMapping("country_mappingGEME3.csv", type = "regional", where = "mrprom") %>% 
    filter(ISO3.Code != "") # iso3-prom-geme3 country mapping
  # DISSAGREGATION TO COUNTRY LEVEL (WORKS)
  rel <- select(mapping, c("ISO3.Code", "GEM.E3.region")) # iso3-geme3 country mapping

  weights <- mbind(
    "Production Level" = GDP,
    "Household Consumption" = GDPpCapita,
    "Total Exports" = GDP,
    "Activity Exports" = GDP
  )
  getNames(weights) <- c(
    "Production Level",
    "Household Consumption",
    "Total Exports",
    "Activity Exports"
  )
  tmp <- toolAggregate(x[, , c("Unit Cost", "End-Use Prices", "Unit Cost Exports")], rel = rel, weight = NULL , from = "GEM.E3.region", to = "ISO3.Code", dim = 1)
  tmp2 <- toolAggregate(x[ , , c("Production Level", "Household Consumption", "Total Exports", "Activity Exports")], rel = rel, weight = weights , from = "GEM.E3.region", to = "ISO3.Code", dim = 1)
  
  x <- mbind(tmp, tmp2)
  
  # find all countries that GEME3 does not have data for
  #noGemData <- getItems(x,1)[is.na(x[,2014,1])]
  
  #FIXME
  
  #rel <- select(mapping, c("ISO3.Code", "GEM.E3.region")) %>% filter(ISO3.Code != "", GEM.E3.region != "")
  #x <- collapseNames(x[, "y2014", "Production Level.Agriculture"])
  #getSets(x)[3] <- "data"
  #x <- toolAggregate(x, rel = rel, weight = gdp[,"y2014",], from = "GEM.E3.region", to = "ISO3.Code", dim = 1)

  suppressMessages(
    suppressWarnings(
      x <- toolCountryFill(x) %>% 
      toolISOhistorical()
    )
  )
  return(x[as.character(getISOlist()), , ]) #nolint
}
