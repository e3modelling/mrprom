#' convertGEME3
#'
#' @param x MAgPIE object.
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
#' @import mrcommons
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'

convertGEME3 <- function(x) {

  # disaggregate data to all ISO3 countries based on their GDP

  gdp <- calcOutput("GDP", aggregate = FALSE) # will use gdp as disaggregation weights
  gdp <- gdp[, getYears(x), , drop = TRUE]
  ISO3.Code <- NULL
  mapping <- toolGetMapping("country_mappingGEME3.csv", type = "regional") %>% # nolint
    filter(ISO3.Code != "") # iso3-prom-geme3 country mapping # nolint
  # DISSAGREGATION TO COUNTRY LEVEL (WORKS)
  rel <- select(mapping, c("ISO3.Code", "GEM.E3.region")) # iso3-geme3 country mapping
  #rel[rel$GEM.E3.region=="","GEM.E3.region"]<-"OTH"
  #tmp <- collapseNames(x[, , "Production Level"])
  #getSets(x)[3] <- "data"
  #tmp<-x["USA",,]
  #getItems(tmp,1) <- "OTH"
  #tmp[,,]=NA
  #x <- mbind(x,tmp)
  tmp <- toolAggregate(x[, , c("Production Level", "Household Consumption")], rel = rel, weight = gdp, from = "GEM.E3.region", to = "ISO3.Code", dim = 1) # nolint
  # TODO: map missing individual countries to other similar countries for Unit Cost (rather than to their region)
  tmp2 <- toolAggregate(x[, , c("Unit Cost", "End-Use Price (Consumption Products)")], rel = rel, weight = NULL, from = "GEM.E3.region", to = "ISO3.Code", dim = 1) # nolint
  x <- mbind(tmp, tmp2)

  # find all countries that GEME3 does not have data for
  #noGemData <- getItems(x,1)[is.na(x[,2014,1])]

  #FIXME

  #rel <- select(mapping, c("ISO3.Code", "GEM.E3.region")) %>% filter(ISO3.Code != "", GEM.E3.region != "")
  #x <- collapseNames(x[, "y2014", "Production Level.Agriculture"])
  #getSets(x)[3] <- "data"
  #x <- toolAggregate(x, rel = rel, weight = gdp[,"y2014",], from = "GEM.E3.region", to = "ISO3.Code", dim = 1)
  x <- toolCountryFill(x) #nolint
  x <- toolISOhistorical(x) #nolint
  return(x[as.character(getISOlist()), , ]) #nolint
}
