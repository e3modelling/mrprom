#' calcEmissionFactors
#'
#' Derive EmissionFactors
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "EmissionFactors", aggregate = TRUE)
#' }
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr filter select

calcEmissionFactors <- function() {
  
  # CO2 emission factors (kgCO2/kgoe fuel burned)
  # https://www.ipcc-nggip.iges.or.jp/public/2006gl/vol2.html?utm_source=chatgpt.com
  # Volume 2, Chapter 1, Table 1.4: “Default CO₂ emission factors for combustion.
  emission_factors <- data.frame(
    fuel = c("CRO", "LGN", "HCL", "GSL", "GDO", "LPG",
             "KRS", "RFO", "OLQ", "NGS", "OGS", "BMSWAS"),
    value = c(3.069,4.229,3.961,2.901,3.102,2.642,
              3.010,3.241,3.069,2.349,1.859,4.187))
  
  IFuelCons2 <- calcOutput(type = "IFuelCons2", aggregate = TRUE)
  
  EDGAR2026 <- readSource("EDGAR2026", subtype = "IPCC 2006 CO2", convert = TRUE)
  EDGAR2026 <- collapseDim(EDGAR2026, dim = 3.4)
  EDGAR2026 <- collapseDim(EDGAR2026, dim = 3.3)
  EDGAR2026 <- collapseDim(EDGAR2026, dim = 3.2)
  
  EDGAR2026toSBS <- toolGetMapping(
    name = "EDGAR2026_to_SBS.csv",
    type = "sectoral",
    where = "mrprom"
  )
  
  EDGAR2026toSBS_first <- EDGAR2026toSBS[
    !duplicated(EDGAR2026toSBS$variable),
  ]
  
  EDGAR2026Unique <- EDGAR2026[,,EDGAR2026toSBS_first$variable]
  
  EDGAR2026Unique <- toolAggregate(EDGAR2026Unique, rel = EDGAR2026toSBS_first, weight = NULL, from = "variable", to = "SBS", dim = 3)
  
  
  list(x = x,
       weight = NULL,
       unit = "MtCO2",
       description = "CO2 emissions factors")
}
