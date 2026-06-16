#' calcTFuelConsShares
#' 
#' @return  OPENPROM input data iFuelConsXXX
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "TFuelConsShares")
#' }
#'
#' @importFrom dplyr filter %>% mutate select last
#' @importFrom tidyr pivot_wider separate_rows crossing
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom eurostat get_eurostat


calcTFuelConsShares <- function(subtype = "TRANSE") {
  
  # compute weights for aggregation by population
  map <- toolGetMapping("regionmappingOPDEV5.csv", "regional", where = "mrprom")
  
  fStartHorizon <- toolReadEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fStartHorizon"]
  
  DOMSE <- readSource("TDOMSEshareproj", subtype = "Shares")
  DOMSE[is.na(DOMSE)] <- 0
  
  INDSEa <- readSource("TSharesINDSE", subtype = "PrimesShares")
  INDSEb <- readSource("TSharesINDSE", subtype = "IEAShares")
  INDSE <- mbind(INDSEa, INDSEb)
  INDSE[is.na(INDSE)] <- 0
  
  x <- mbind(DOMSE, INDSE)
  
  w <- calcOutput(type = "IFuelCons2", aggregate = FALSE)
  w <- add_columns(w, addnm = setdiff(getItems(x,3),getItems(w,3)), dim = 3, fill = 0)
  w <- w[,2023,]
  w <- collapseDim(w, 2)
  w <- w[,, getItems(x, 3)]
  
  x <- toolAggregate(x, rel = map, weight = NULL, from = "Region.Code", to = "ISO3.Code", dim = 1)
  
  list(x = x,
       weight = w,
       unit = "shares",
       description = "fuel consumption targets shares")
  
}
