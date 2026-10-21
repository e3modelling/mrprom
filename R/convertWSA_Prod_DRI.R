#' convertWSA_Prod_DRI
#'
#' The ISO codes of "WSA_Prod_DRI" data are compared with the official ISO code country list.
#' NA values are replaced with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "WSA_Prod_DRI" data with spatial entries for each country.
#'
#' @author Margarita Efthymiadou
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("WSA_Prod_DRI", convert = TRUE)
#' }
#'
convertWSA_Prod_DRI <- function(x) {

  map <- toolGetMapping(
    name = "aggregate_regions_WSA_Prod_DRI.csv",
    type = "regional",
    where = "mrprom"
  )
  
  regions <- x[unique(map[, "Region"]),,]
  
  fuel <- calcOutput(type = "IFuelCons2", aggregate = FALSE)
  fuel <- dimSums(fuel[,,"IS"], 3)
  
  fuel[fuel==0] <- 1e-6
  
  fuel <- fuel[,getYears(x),]
  
  regions1 <- regions[,,"Total_Production.Mt"]
  regionsa <- toolAggregate(
    regions1,
    weight = fuel,
    rel = map,
    from = "Region",
    to = "ISO3.Code"
  )
  
  regionsb <- regions[,,"DRI_Production.Mt"]
  regionsb <- toolCountryFill(regionsb, fill = 0)
  
  regions3 <- regions[,,setdiff(getItems(regions,3),c("Total_Production.Mt","DRI_Production.Mt"))]
  regionsc <- toolAggregate(
    regions3,
    weight = NULL,
    rel = map,
    from = "Region",
    to = "ISO3.Code"
  )
  
  regions <- mbind(regionsa, regionsb, regionsc)
  
  x <- as.quitte(x)

  suppressWarnings({
    levels(x[["region"]]) <-toolCountry2isocode(levels(x[["region"]]), mapping =
                                                  c("Taiwan, China" = "TWN"))
  })

  x <- filter(x, !is.na(x[["region"]]))
  x <- as.magpie(x)
  
  # Find regions missing from x
  missing_regions <- setdiff(getRegions(regions), getRegions(x))
  
  # Add only missing regions
  x <- mbind(x, regions[missing_regions, , ])

  suppressMessages(
    suppressWarnings(
      x <- toolCountryFill(x, fill = NA)
    )
  )
  return(x[as.character(getISOlist()), , ])
}
