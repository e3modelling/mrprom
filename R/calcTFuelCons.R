#' calcTFuelCons
#'
#' @return  OPENPROM input targets iFuelCons
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "TFuelCons")
#' }
#'
#' @importFrom dplyr filter %>% mutate select last
#' @importFrom tidyr pivot_wider separate_rows crossing
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom eurostat get_eurostat


calcTFuelCons <- function() {
  
  # compute weights for aggregation by population
  map <- toolGetMapping("regionmappingOPDEV5.csv", "regional", where = "mrprom")
  
  fStartHorizon <- toolReadEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fStartHorizon"]
  
  x <- readSource("TDOMSEshareproj", subtype = "Projections")
  a <- readSource("TSharesINDSE", subtype = "PrimesProjections")
  b <- readSource("TSharesINDSE", subtype = "IEAProjections")
  z <- mbind(a, b)
  x <- mbind(x, z)
  
  w <- calcOutput(type = "IFuelCons2", aggregate = FALSE)
  w <- w[,2023,]
  w <- dimSums(w, 3.2)
  w <- collapseDim(w, 2)
  w <- w[,, getItems(x, 3)]
  
  x <- toolAggregate(x, rel = map, weight = w, from = "Region.Code", to = "ISO3.Code", dim = 1)
  
  list(x = x,
       weight = NULL,
       unit = "various",
       description = "fuel consumption targets")
  
}
