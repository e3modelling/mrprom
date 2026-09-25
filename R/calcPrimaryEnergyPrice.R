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
  
  fStartHorizon <- toolReadEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fStartHorizon"]
  
  fEndY <- toolReadEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fEndY"]
  
  fEndHorizon <- toolReadEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fEndHorizon"]
  
  a <- readSource("IEA_WEO_2025_ExtendedData", subtype = "Prices", convert = FALSE)
  Historical <- a[,,c("Historical")]
  Projections <- a[,,c("Current Policies Scenario")]
  
  histCRO <- Historical[,,"IEA crude oil ($/barrel)"]["WORLD",,]
  ProjCRO <- Projections[,,"IEA crude oil ($/barrel)"]["WORLD",,]
  
  CRO <- mbind(histCRO, ProjCRO)
  
  CRO <- CRO %>%
    as.quitte() %>%
    filter(!is.na(value)) %>%
    select(- scenario)  %>% as.quitte() %>%
    as.magpie()
  
  CRO <- add_columns(CRO, addnm = "DEU", dim = "region", fill = NA)
  CRO["DEU",,] <- CRO["WORLD",,]
  CRO <- toolCountryFill(CRO, fill = NA)
  CRO[setdiff(getISOlist(),"DEU"),,] <- CRO["DEU",,]
  getItems(CRO, 3.1) <- "CRO"
  # 1 toe ≈ 7.33 barrels of crude oil
  # 2024 -> 2015 0.8
  CRO <- CRO * 7.33 * 0.8  / 1000
  getItems(CRO, 3.2) <- "kUSD2015/toe"
  getItems(CRO, 3.1) <- "CRO"
  
  histCOAL <- Historical[,,"Steam coal ($/tonne)"]
  ProjCOAL <- Projections[,,"Steam coal ($/tonne)"]
  
  COAL <- mbind(histCOAL, ProjCOAL)
  
  COAL <- COAL %>%
    as.quitte() %>%
    filter(!is.na(value)) %>%
    select(- scenario)  %>% as.quitte() %>%
    as.magpie()
  
  getItems(COAL, 1)[getItems(COAL, 1) == "Coastal China"] <- "China"
  
  OPENPROM_4regions <- toolGetMapping("regionmapping_OPENPROM_4regions.csv", "regional", where = "mrprom")
  
  COAL <- toolAggregate(COAL, rel = OPENPROM_4regions, dim =1 , from = "Region.Code", to = "ISO3.Code")
  # 0.7 toe per tonne
  COAL <- COAL / 0.7 * 0.80 / 1000
  getItems(COAL, 3.2) <- "kUSD2015/toe"
  getItems(COAL, 3.1) <- "HCL"
  
  histNGS <- Historical[,,"Natural gas ($/MBtu)"]
  ProjNGS <- Projections[,,"Natural gas ($/MBtu)"]
  
  NGS <- mbind(histNGS, ProjNGS)
  
  NGS <- NGS %>%
    as.quitte() %>%
    filter(!is.na(value)) %>%
    select(- scenario)  %>% as.quitte() %>%
    as.magpie()
  
  getItems(NGS, 1)[getItems(NGS, 1) == "Coastal China"] <- "China"
  
  OPENPROM_4regions <- toolGetMapping("regionmapping_OPENPROM_4regions.csv", "regional", where = "mrprom")
  
  NGS <- toolAggregate(NGS, rel = OPENPROM_4regions, dim =1 , from = "Region.Code", to = "ISO3.Code")
  # 1 toe ≈ 39.68 MBtu
  NGS <- NGS * 39.68 * 0.80 / 1000
  getItems(NGS, 3.2) <- "kUSD2015/toe"
  getItems(NGS, 3.1) <- "NGS"
  
  BMSWAS_Price <- readSource("MAgPIE_BMSWAS_Price")
  BMSWAS <- BMSWAS_Price[,,"IS.kUSD2015/toe"]
  getItems(BMSWAS, 3.1) <- "BMSWAS"
  
  map <- toolGetMapping("regionmappingOPDEV5.csv", "regional", where = "mrprom")
  BMSWAS <- toolAggregate(BMSWAS, dim=1, rel = map, from="Region.Code", to="ISO3.Code")

  IEA <- mbind(CRO, COAL, NGS)
  
  qx <- as.quitte(IEA) %>%
    interpolate_missing_periods(period = c(fStartHorizon : fEndHorizon), expand.values = TRUE)
  
  x <- as.magpie(qx)
  
  x <- mbind(x, BMSWAS)
  
  x <- x[,fStartHorizon : fEndHorizon,]
  
  #-----------------------weights------------------------
  TES <- calcOutput(type = "ITotEneSupply", subtype = "TES", aggregate = FALSE)
  TES <- TES[,2023,getItems(x,3.1)]
  weights <- x
  weights[, , ] <- TES
  weights[c("CYP","EST","LUX","LVA","MLT","SVN","CYP"),,] <- weights["DEU",,] 
  
  list(x = x,
       weight = weights,
       unit = "kUSD2015/toe",
       description = "Primary Energy Price")
  
}
