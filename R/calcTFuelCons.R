#' calcTFuelCons
#'
#' Use ENERDATA, IEA, TREMOVE and NAVIGATE fuel consumption data to derive
#' OPENPROM input parameter iFuelConsXXX
#' The data for the years 2010 : 2021 is mainly from ENARDATA and IEA. 
#' For the years 2021:2100 the data is mainly from NAVIGATE.
#' For TRANSE the data from 2021:2100 the data is mainly from TREMOVE.
#' (XXX: NENSE, INDSE, DOMSE, TRANSE). 
#'
#' @param subtype string, OPENPROM sector (DOMSE, INDSE, NENSE, TRANSE)
#' @return  OPENPROM input data iFuelConsXXX
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "TFuelCons", subtype = "DOMSE", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select last
#' @importFrom tidyr pivot_wider separate_rows crossing
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom eurostat get_eurostat


calcTFuelCons <- function(subtype = "TRANSE") {
  
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
