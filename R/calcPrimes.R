#' calcPrimes
#'
#' Use Primes data for fuel consumption in DOMSE, INDSE, NENSE, TRANSE
#' 
#' @return Primes fuel consumption in DOMSE, INDSE, NENSE, TRANSE
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "Primes", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select full_join
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom utils tail


calcPrimes <- function() {
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  # load current OPENPROM set configuration
  sets <- toolGetMapping(paste0("TRANSE.csv"),
                         type = "blabla_export",
                         where = "mrprom")
  
  sets <- as.character(sets[, 1])
  
  #PrimesTransport data
  a <- readSource("PrimesNewTransport")
  
  a <- a[getRegions(a)[getRegions(a) %in% as.character(getISOlist())], , ]
  
  a <- a / 1000 #ktoe to mtoe
  
  #add Passenger Light Duty Vehicles to PC
  PC <- a[,,"Passenger Light Duty Vehicles"]
  
  getItems(a,3.3) <- "Mtoe"
  getItems(a,3.5) <- "Final Energy Demand"
  
  map_TREMOVE <- toolGetMapping(name = "prom-TREMOVE-fucon-mapping.csv",
                                type = "sectoral",
                                where = "mrprom")
  
  #remove the empty cells from mapping
  map_TREMOVE <- map_TREMOVE[!(map_TREMOVE[, "FUEL"] == ""), ]
  
  map_TREMOVE <- filter(map_TREMOVE, map_TREMOVE[, "SBS"] %in% sets)
  
  a <- toolAggregate(a[, , as.character(unique(map_TREMOVE[["FUEL"]]))], dim = 3.2, rel = map_TREMOVE, from = "FUEL", to = "EF")
  a <- toolAggregate(a[, , as.character(unique(map_TREMOVE[["TREMOVE"]]))], dim = 3.4, rel = map_TREMOVE, from = "TREMOVE", to = "SBS")
  
  PC <- toolAggregate(PC[, , as.character(unique(map_TREMOVE[["FUEL"]]))], dim = 3.2, rel = map_TREMOVE, from = "FUEL", to = "EF")
  getItems(PC,3.4) <- "PC"
  
  a[,,"PC"] <- a[,,"PC"] + ifelse(is.na(PC), mean(PC, na.rm=TRUE), PC)
  
  a <- as.quitte(a)
  
  names(a) <- sub("variable", "new", names(a))
  names(a) <- sub("technology", "variable", names(a))
  
  a <- select(a, -("sector"))
  
  a[,"scenario"] <- "(Missing)"
  
  a <-  as.quitte(a) %>%
    interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
  
  x <- as.quitte(a) %>% as.magpie()
  
  suppressMessages(
    suppressWarnings(
      x <- toolCountryFill(x, fill = NA)
    )
  )
  
  # set NA to 0
  x[is.na(x)] <- 10^-6
  x_TRANSE <- x[,fStartHorizon : 2100,]
  
  b <- readSource("PrimesBalances")
  
  b <- b[,,c("IS","NF","PCH","CH","OI","PP","FD","TX","HOU","SE","AG","EN","NEN")]
  
  mapping <- list(
    primes = c(
      "hard coal", "patent fuels", "coke", "tar,pitch,benzol", "lignite", "other solids",
      "Crude oil", "Feedstocks",
      "refinery gas", "liqufied petroleum gas", "gasoline", "kerosene", 
      "naptha", "diesel oil", "fuel oil", "other liquids",
      "natural gas incl_ clean gas", "coke-oven gas", "blast furnace gas","gasworks gas",
      "biomass-waste", "nuclear", "hydro", "wind", 
      "solar", "tidal and other renewables", "geothermal heat", 
      "methanol", "ethanol", "hydrogen (incl_ distributed and directly used)", 
      "steam", "electricity"
    ),
    openprom = c(
      "HCL","HCL", "HCL", "HCL", "LGN", "HCL",
      "CRO", "CRO",
      "OLQ", "LPG", "GSL", "KRS",
      "OLQ", "GDO","RFO", "OLQ",
      "NGS", "OGS","OGS", "OGS",
      "BMSWAS", "NUC" ,"HYD", "WND", "SOL",
      "GEO", "GEO", "MET", "ETH", "H2F", "STE", "ELC"
    )
  )
  
  mapping <- as.data.frame(mapping)
  
  b <- toolAggregate(b[, , as.character(unique(mapping[["primes"]]))], dim = 3.4, rel = mapping, from = "primes", to = "openprom")
  
  b <- b[getRegions(b)[getRegions(b) %in% as.character(getISOlist())], , ]
  
  b <- b / 1000 #ktoe to mtoe
  
  getItems(b,3.3) <- "Mtoe"

  b <- as.quitte(b)
  
  names(b) <- sub("fuel", "new", names(b))
  
  b[,"scenario"] <- "(Missing)"
  
  b <-  as.quitte(b) %>%
    interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
  
  b <- as.quitte(b) %>% as.magpie()
  
  suppressMessages(
    suppressWarnings(
      b <- toolCountryFill(b, fill = NA)
    )
  )
  
  # set NA to 0
  b[is.na(b)] <- 10^-6
  b_Primes <- b[,fStartHorizon : 2100,]
  
  x <- mbind(x_TRANSE, b_Primes)
  
  list(x = x,
       weight = NULL,
       unit = "Mtoe",
       description = "Primes fuel consumption in DOMSE, INDSE, NENSE, TRANSE")
  
}
