#' calcPrimesPrices
#'
#' Use Primes Prices data for END USER PRICE
#' 
#' @return Primes Prices data for END USER PRICE
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "PrimesPrices", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select full_join
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom utils tail


calcPrimesPrices <- function() {
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  #PrimesPrices data
  a <- readSource("PrimesPrices")
  
  a <- a[,,"END USER PRICE (in €/toe)"]
  
  a <- a[getRegions(a)[getRegions(a) %in% as.character(getISOlist())], , ]
  
  mapping <- list(
    primes = c(
      "Diesel oil","Gasoline","Fuel oil", "LPG", "Byproducts",
      "Naptha", "Other liquid fuels", "Kerosene", "Natural gas", "Solids",
      "Biomass", "Waste","Methanol","Ethanol","H2F",
      "Electricity","Steam","Hydrogen","Fossil Diesel","Biodiesel",        
      "Synthetic Diesel","Fossil Gasoline","Biogasoline","Synthetic Gasoline","Fossil Kerosene",  
      "Biokerosene","Synthetic Kerosene","Fossil Fuel Oil","Bioheavy","Synthetic Fuel Oil",
      "Fossil Natural gas","Biogas","Hydrogen Blended","Clean Gas" 
    ),
    openprom = c(
      "GDO","GSL","RFO", "LPG", "Byproducts",
      "Naptha", "OLQ", "KRS", "NGS", "SLD",
      "BMSWAS", "Waste","MET","ETH","H2F",
      "ELC","STE","Hydrogen","Fossil Diesel","BGDO",        
      "Synthetic Diesel","Fossil Gasoline","Biogasoline","Synthetic Gasoline","Fossil Kerosene",  
      "Biokerosene","Synthetic Kerosene","Fossil Fuel Oil","Bioheavy","Synthetic Fuel Oil",
      "Fossil Natural gas","Biogas","Hydrogen Blended","Clean Gas"
    )
  )
  
  mapping2 <- list(
    primes = c(
      "Power generation","Industry","Households","Services","Agriculture",      
      "Transport private","Transport public","Rail","Inland navigation","Hard coal - PG",   
      "lignite - PG","Iron and steel","Other industries","Tertiary","Average price"
    ),
    openprom = c(
      "PG","INDSE","HOU","SE","AG",      
      "Transport private","Transport public","Rail","Inland navigation","Hard coal - PG",   
      "lignite - PG","IS","OI","DOMSE","Average price"
    )
  )
  
  TRANSE <- toolGetMapping(paste0("TRANSE.csv"),
                         type = "blabla_export",
                         where = "mrprom")
  
  DOMSE <- toolGetMapping(paste0("DOMSE.csv"),
                         type = "blabla_export",
                         where = "mrprom")
  
  INDSE <- toolGetMapping(paste0("INDSE.csv"),
                         type = "blabla_export",
                         where = "mrprom")
  
  NENSE <- toolGetMapping(paste0("NENSE.csv"),
                         type = "blabla_export",
                         where = "mrprom")
  
  names(TRANSE) <- "SBS"
  names(DOMSE) <- "SBS"
  names(INDSE) <- "SBS"
  names(NENSE) <- "SBS"
  
  SBS <- rbind(TRANSE,DOMSE,INDSE,NENSE)
  
  SBS <- as.character(SBS[, 1])
  
  SBS <- c(SBS,"INDSE","DOMSE","Average price","PG")
  
  mapping2 <- as.data.frame(mapping2)
  
  mapping2 <- filter(mapping2, mapping2[,"openprom"] %in% SBS)
  
  
  EFS <- toolGetMapping(paste0("EFS.csv"),
                          type = "blabla_export",
                          where = "mrprom")
  
  mapping <- as.data.frame(mapping)
  
  EFS <- as.character(EFS[, 1])
  EFS <- c(EFS,"SLD","OLQ")
  
  mapping <- filter(mapping, mapping[,"openprom"] %in% EFS)
  
  a <- toolAggregate(a[, , as.character(unique(mapping2[["primes"]]))], dim = 3.2, rel = mapping2, from = "primes", to = "openprom")
   
  mapping <- filter(mapping, mapping[,"primes"] %in% getItems(a,3.4))
  
  a <- toolAggregate(a[, , as.character(unique(mapping[["primes"]]))], dim = 3.4, rel = mapping, from = "primes", to = "openprom")
  
  a <- as.quitte(a)
  
  a <-  as.quitte(a) %>%
    interpolate_missing_periods(period = fStartHorizon : 2070, expand.values = TRUE)
  
  x <- as.quitte(a) %>% as.magpie()
  
  suppressWarnings({
    x <- toolCountryFill(x, fill = NA)
  })

  # set NA to 0
  x[is.na(x)] <- 10^-6
  
  
  list(x = x,
       weight = NULL,
       unit = "€'15/toe",
       description = "Primes Prices data for END USER PRICE")
  
}
