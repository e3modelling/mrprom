#' calcTDemand
#'
#' Use Primes, IEA data for Secondary Energy Electricity per year.
#' Info:
#' Primes: Trends for Secondary Energy Electricity data, EU countries until 2070
#' IEA: Trends for Secondary Energy Electricity data, 225 countries until 2050.
#' The trends are the same for each country depending to the region. For example
#' HKG and CHN have the same trends for capacity, 225 countries until 2050.
#' IEA mapping: "Africa" = "SSA", "Middle East" = "MEA", "Eurasia" = "REF",
#' "Southeast Asia" = "OAS", "Central and South America" = "LAM",
#' "Asia Pacific" = "CAZ", "Europe" = "NEU", "European Union" = "ELL"
#' calculate CAZ, NEU and ELL 
#' "CAZ" <- "CAZ" -  "OAS"
#' ELL and NEU have the same trends
#' IEA_non_EU <- "NEU" - "ELL"
#' "NEU" <- IEA_non_EU
#' "ELL" <- IEA_non_EU
#' The trends are multiplied with the
#' calcOutput(type = "IDataElecProd", mode = "Total", aggregate = FALSE) 
#' data to find the Secondary Energy Electricity data.
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