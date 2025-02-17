#' convertIEA_Industry_Roadmaps
#'
#' 
#' @param x MAgPIE object.
#'
#' @return blablabla
#'
#' @author Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_Industry_Roadmaps", convert = TRUE)
#' }
#'
#'
convertIEA_Industry_Roadmaps <- function(x) {

#  map <- toolGetMapping("regionmappingH12.csv", where = "madrat")
#  eu <- toolAggregate(x["EUR", , ], rel = map, partrel = TRUE)
  
  getItems(x,1) <- toolCountry2isocode(getItems(x,1),
                                               mapping = c("European Union" = "EUR",
                                                           "Middle East" = "MEA"))
  x <- toolCountryFill(x, fill = 0)

  return(x)

}
