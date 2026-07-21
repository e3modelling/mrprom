#' calcPrimaryEnergyPrice
#' 
#' @return  OPENPROM input data PrimaryEnergyPrice
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "PrimaryEnergyPrice")
#' }
#'
#' @importFrom dplyr filter %>% mutate select last
#' @importFrom tidyr pivot_wider separate_rows crossing
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom eurostat get_eurostat


calcPrimaryEnergyPrice <- function() {
  
  EFS <- toolGetMapping(
    name = "EFS.csv",
    type = "blabla_export",
    where = "mrprom"
  ) %>%
    separate_rows(EFS, sep = ",")
  
  fStartHorizon <- toolReadEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fStartHorizon"]
  
  fEndY <- toolReadEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fEndY"]
  
  variable <- EFS
  period <- fStartHorizon:fEndY
  unit <- "kUSD2015/toe"
  
  df <- tidyr::expand_grid(
    region = getISOlist(),
    variable = variable,
    period = period,
    unit = unit
  ) %>%
    dplyr::mutate(value = NA_real_)
  
  BMSWAS_Price <- readSource("MAgPIE_BMSWAS_Price")
  BMSWAS <- BMSWAS_Price[,,"IS.kUSD2015/toe"]
  getItems(BMSWAS, 3.1) <- "BMSWAS"
  
  map <- toolGetMapping("regionmappingOPDEV5.csv", "regional", where = "mrprom")
  BMSWAS <- toolAggregate(BMSWAS, dim=1, rel = map, from="Region.Code", to="ISO3.Code")
  
  CRO <- readSource("IEACrudeOilPrice")
  CRO <- add_columns(CRO, addnm = "DEU", dim = "region", fill = NA)
  CRO["DEU",,] <- CRO["WORLD",,]
  CRO <- toolCountryFill(CRO, fill = NA)
  CRO[setdiff(getISOlist(),"DEU"),,] <- CRO["DEU",,]
  getItems(CRO, 3.1) <- "CRO"
  getItems(CRO, 3.2) <- "kUSD2015/toe"
  
  x <- mbind(BMSWAS, CRO)
  
  x <- x[,fStartHorizon : fEndY,]
  
  list(x = x,
       weight = NULL,
       unit = "kUSD2015/toe",
       description = "Primary Energy Price")
  
}
