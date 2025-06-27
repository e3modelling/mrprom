#' calcPrimesPG
#'
#' Use Primes data Secondary Energy Electricity, Gross Electricity generation by plant type
#' and Net Installed Power Capacity
#' 
#' @param subtype "SE" for SE Electricity or "power generation" for
#' Gross Electricity generation by plant type and "capacity" for
#' Net Installed Power Capacity
#' 
#' @return Primes PG data
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "PrimesPG",subtype = "SE", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select full_join
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom utils tail


calcPrimesPG <- function(subtype = "SE") {
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  #Primes data
  a <- readSource("PrimesPGData", subtype = subtype)
  
  if (subtype %in% c("power generation", "capacity")) {
    mapping <- list(
      primes = c(
        "Nuclear energy","Lakes","Run of river", "Wind on-shore",
        "Wind off-shore", "Solar","Solids fired", "Oil fired", "Gas fired",
        "Biomass-waste fired","Geothermal heat"),
      openprom = c(
        "PGANUC","PGLHYD","PGSHYD", "PGAWND",
        "PGAWNO", "PGSOL","ATHCOAL", "ATHOIL", "ATHGAS",
        "ATHBMSWAS", "PGOTHREN"))
    
    mapping <- as.data.frame(mapping)
    
    a <- toolAggregate(a[, , as.character(unique(mapping[["primes"]]))], dim = 3.2, rel = mapping, from = "primes", to = "openprom")
  }
  
  a <- a[getRegions(a)[getRegions(a) %in% as.character(getISOlist())], , ]
  a <- toolCountryFill(a, fill = NA)
  
  a <-  as.quitte(a) %>%
    interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
  
  a <- as.quitte(a) %>% as.magpie()
  
  a <- toolCountryFill(a, fill = NA)
  
  # set NA to 0
  a[is.na(a)] <- 10^-6
  a <- a[,fStartHorizon : 2100,]
  
  list(x = a,
       weight = NULL,
       unit = "various",
       description = "Primes Secondary Energy Electricity Gross Electricity generation by plant type
      and Net Installed Power Capacity")
  
}
