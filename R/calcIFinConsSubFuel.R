#' calcIFinConsSubFuel
#'
#' Use calcFuelConsumption to derive OPENPROM input data iFinConsSubFuel
#' (fuel consumption in NENSE sectors, i.e. PCH,NEN,BU - non energy and bunkers)
#'
#'
#' @return  OPENPROM input data iFinConsSubFuel
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IFinConsSubFuel", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>%
#' @importFrom quitte as.quitte


calcIFinConsSubFuel <- function() {

  nen <- calcOutput("FuelConsumption", subtype = "non energy", aggregate = FALSE)
  bun <- calcOutput("FuelConsumption", subtype = "bunkers", aggregate = FALSE)
  pch <- calcOutput("FuelConsumption", subtype = "chemical feedstock", aggregate = FALSE)
  x <- mbind(nen, bun, pch)
  map <- toolGetMapping("prom-enerdata-fucon-mapping.csv", "sectoral")
  x <- x[, , map[!is.na(map[, 1]), 1]]
  getNames(x)<-paste0(paste(map[c(3,4,7),2],map[c(3,4,7),3],sep="."),".",sub("^.*.\\..*.\\.","",getNames(tmp)))
  #duplicated(sub("\\..*.$","",getNames(x))) CHECK MISSING DATA due to different units
  x <-as.quitte(x)
  x <- as.magpie(x)
  u <- getItems(x, "unit")

  list(x = x,
       weight = NULL,
       unit = u,
       description = "Enerdata; fuel consumption in NENSE sectors, i.e. PCH,NEN,BU - non energy and bunkers")

}
