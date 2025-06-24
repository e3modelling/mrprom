#' calcUN
#'
#' Use UN data for fuel consumption in DOMSE, INDSE, NENSE, TRANSE
#' 
#' @return UN fuel consumption in DOMSE, INDSE, NENSE, TRANSE
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "UN", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select full_join
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom utils tail


calcUN <- function() {
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  #UN data
  x <- readSource("UN")
  
  x <- x[,,getItems(a,3.3)[!(getItems(a,3.3) %in% c("FE","REN"))]]
  
  x <- x[,,c("CH","NF","BM","FD","PP","TX","EN","OI","SE","AG","HOU","IS","NEN","PT","PN","PC","PA")]
  
  a6 <- readSource("IRF", subtype = "inland-surface-passenger-transport-by-rail")
  #million pKm/yr
  a7 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-rail")
  #million tKm/yr
  a6 <- a6[, Reduce(intersect, list(getYears(a6), getYears(a7), getYears(x))), ]
  a7 <- a7[, Reduce(intersect, list(getYears(a6), getYears(a7), getYears(x))), ]
  x <- x[, Reduce(intersect, list(getYears(a6), getYears(a7), getYears(x))), ]
  
  out3 <- (a6 / (a6 + a7))
  out4 <- (a7 / (a6 + a7))
  
  x <- add_columns(x, addnm = "GT", dim = 3.1, fill = NA)
  
  #inland-surface-freight-transport-by-rail / total inland-surface
  x[, , "GT"] <- x[, , "PT"] * ifelse(is.na(out4), mean(out4, na.rm=TRUE), out4)
  
  #inland-surface-passenger-transport-by-rail / total inland-surface transport-by-rail
  x[, , "PT"] <- x[, , "PT"] * ifelse(is.na(out3), mean(out3, na.rm=TRUE), out3)
  

  
  
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
  
  x <- toolCountryFill(x, fill = NA)
  # set NA to 0
  x[is.na(x)] <- 10^-6
  x_TRANSE <- x[,fStartHorizon : 2100,]
  
  b <- readSource("PrimesBalances")
  
  b <- b[,,c("IS","NF","PCH","CH","OI","PP","FD","TX","HOU","SE","AG","EN")]
  
  mapping <- list(
    primes = c(
      "hard coal","coke","patent fuels", "lignite", "Crude oil", "Feedstocks",
      "liqufied petroleum gas", "gasoline", "kerosene", "diesel oil", "fuel oil",
      "other liquids", "natural gas incl_ clean gas","coke-oven gas",
      "blast furnace gas", "biomass-waste", "nuclear","hydro", "wind", "solar",
      "geothermal heat", "methanol", "ethanol",
      "hydrogen (incl_ distributed and directly used)", "steam", "electricity"
    ),
    openprom = c(
      "HCL","HCL", "HCL", "LGN", "CRO", "CRO",
      "LPG", "GSL", "KRS", "GDO", "RFO",
      "OLQ","NGS","OGS", "OGS", "BMSWAS", "NUC","HYD", "WND", "SOL",
      "GEO", "MET", "ETH",
      "H2F", "STE", "ELC"
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
  
  b <- toolCountryFill(b, fill = NA)
  # set NA to 0
  b[is.na(b)] <- 10^-6
  b_Primes <- b[,fStartHorizon : 2100,]
  
  x <- mbind(x_TRANSE, b_Primes)
  
  list(x = x,
       weight = NULL,
       unit = "Mtoe",
       description = "UN fuel consumption in DOMSE, INDSE, NENSE, TRANSE")
  
}
