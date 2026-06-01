#' calcTDemand
#'
#' Derive yearly Secondary Energy Electricity demand
#'
#' Computes annual Secondary Energy|Electricity values by applying
#' region-specific electricity demand trends to baseline electricity
#' production data. For EU countries, trend pathways are derived from
#' PRIMES projections up to 2070, while for non-EU regions, trends are
#' based on IEA projections available up to 2050. IEA regional pathways
#' are mapped to the OPEN-PROM regional structure and extended to all
#' countries within each region. The resulting trend factors are applied
#' to baseline electricity production from `IDataElecProd` to generate
#' annual Secondary Energy|Electricity values in TWh for OPEN-PROM.
#' 
#' @return Secondary Energy Electricity per year
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "TDemand", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select full_join arrange group_by distinct intersect setdiff ungroup group_map 
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom utils tail


calcTDemand <- function() {
  
  a <- calcOutput(type = "TProdElec", aggregate = FALSE)
  
  a <- dimSums(a,3)
  a <- collapseDim(a, 3)
  
  a <- add_dimension(a, dim = 3.1, add = "variable", nm = "Secondary Energy|Electricity")
  a <- add_dimension(a, dim = 3.2, add = "unit", nm = "TWh")
  
  list(x = a,
       weight = NULL,
       unit = "TWh",
       description = "Primes,IEA Secondary Energy Electricity per year")
  
}