#' calcIFuelCons
#'
#' Use ENERDATA, IEA, TREMOVE and NAVIGATE fuel consumption data to derive
#' OPENPROM input parameter iFuelConsXXX
#' The data for the years 2010 : 2021 is mainly from ENARDATA and IEA. 
#' For the years 2021:2100 the data is mainly from NAVIGATE.
#' For TRANSE the data from 2021:2100 the data is mainly from TREMOVE.
#' (XXX: NENSE, INDSE, DOMSE, TRANSE). 
#'
#' @param subtype string, OPENPROM sector (DOMSE, INDSE, NENSE, TRANSE)
#' @return  OPENPROM input data iFuelConsXXX
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IFuelCons", subtype = "DOMSE", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select last
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte
#' @importFrom utils tail
#' @importFrom R.utils isZero


calcIFuelCons <- function(subtype = "DOMSE") {

  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]

  # load current OPENPROM set configuration
  sets <- toolGetMapping(paste0(subtype, ".csv"),
                         type = "blabla_export",
                         where = "mrprom")
  
  sets <- as.character(sets[, 1])

  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-enerdata-IEA-fucon-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")
  maps <- map
  ## filter mapping to keep only XXX sectors
  map <- filter(map, map[, "SBS"] %in% sets)
  
  IEA <- NULL
  map <- map %>% drop_na(IEA)
  
  #the map has a column SBS which corresponds to flow of IEA
  for (ii in unique(map[, "flow"])) {
    
    d <- readSource("IEA2025", subtype = as.character(ii))
    d <- d[,,"KTOE"]
    getItems(d,3.1) <- "Mtoe"
    d <- d / 1000 #ktoe to mtoe

    #each flow has some products as it is the EF column of map
    m <- filter(map, map[["flow"]] == ii)
    
    flow_IEA <- getItems(d, 3.2)[getItems(d, 3.2) %in% m[,"IEA"]]
    
    qy <- d[,,flow_IEA]
    
    qy <-qy[,fStartHorizon : max(getYears(d, as.integer = TRUE)),]
    
    IEA <- mbind(IEA, qy)
  }
  
  IEA <- as.quitte(IEA)
  names(IEA) <- sub("product", "new", names(IEA))
  IEA <- select(IEA, -"variable")
  names(IEA) <- sub("flow", "variable", names(IEA))
  
  IEA <- as.quitte(IEA)  %>% as.magpie()
  
  map_inc <- map[map[,"IEA"] %in% getItems(IEA, 3.3),]
  IEA <- toolAggregate(IEA,dim = 3.1,rel = map_inc,from = "flow",to = "SBS")
  IEA <- toolAggregate(IEA,dim = 3.3,rel = map_inc,from = "IEA",to = "EF")
  
  x <- IEA
  
  if (subtype == "TRANSE") {

    a6 <- readSource("IRF", subtype = "inland-surface-passenger-transport-by-rail")
    #million pKm/yr
    a7 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-rail")
    #million tKm/yr
    a6 <- a6[, Reduce(intersect, list(getYears(a6), getYears(a7), getYears(x))), ]
    a7 <- a7[, Reduce(intersect, list(getYears(a6), getYears(a7), getYears(x))), ]
    x <- x[, Reduce(intersect, list(getYears(a6), getYears(a7), getYears(x))), ]
    
    out3 <- (a6 / (a6 + a7))
    out4 <- (a7 / (a6 + a7))

    #inland-surface-passenger-transport-by-rail / total inland-surface transport-by-rail
    x[, , "PT"][,,"GDO"] <- x[, , "PT"][,,"GDO"] * ifelse(is.na(out3), mean(out3, na.rm=TRUE), out3)
    #inland-surface-freight-transport-by-rail / total inland-surface
    x[, , "GT"][,,"GDO"] <- x[, , "GT"][,,"GDO"] * ifelse(is.na(out4), mean(out4, na.rm=TRUE), out4)

    x[, , "PT"][,,"ELC"] <- x[, , "PT"][,,"ELC"] * ifelse(is.na(out3), mean(out3, na.rm=TRUE), out3)
    x[, , "GT"][,,"ELC"] <- x[, , "GT"][,,"ELC"] * ifelse(is.na(out4), mean(out4, na.rm=TRUE), out4)

    a8 <- readSource("IRF", subtype = "passenger-car-traffic")
    #million motor vehicles Km/yr
    a9 <- readSource("IRF", subtype = "total-four-wheeled-traffic")
    #million motor vehicles Km/yr
    a8 <- a8[, Reduce(intersect, list(getYears(a8), getYears(a9), getYears(x))), ]
    a9 <- a9[, Reduce(intersect, list(getYears(a8), getYears(a9), getYears(x))), ]
    x <- x[, Reduce(intersect, list(getYears(a8), getYears(a9), getYears(x))), ]
    
    out1 <- (a8 / a9)
    
    a10 <- readSource("IRF", subtype = "total-van,-pickup,-lorry-and-road-tractor-traffic")
    #million motor vehicles Km/yr
    a11 <- readSource("IRF", subtype = "total-four-wheeled-traffic")
    #million motor vehicles Km/yr
    a10 <- a10[, Reduce(intersect, list(getYears(a10), getYears(a11), getYears(x))), ]
    a11 <- a11[, Reduce(intersect, list(getYears(a10), getYears(a11), getYears(x))), ]
    x <- x[, Reduce(intersect, list(getYears(a10), getYears(a11), getYears(x))), ]
    
    out2 <- (a10 / a11)
    
    #passenger-car-traffic / total-van,-pickup,-lorry-and-road-tractor-traffic
    x[, , "PC"][, , "GDO"] <- x[, , "PC"][, , "GDO"] * ifelse(is.na(out1), mean(out1, na.rm=TRUE), out1)
    x[, , "PC"][,,"GSL"] <- x[, , "PC"][,,"GSL"] * ifelse(is.na(out1), mean(out1, na.rm=TRUE), out1)
    x[, , "GU"][, , "GDO"] <- x[, , "GU"][, , "GDO"] * ifelse(is.na(out2), mean(out2, na.rm=TRUE), out2)
    
    #PB
    a12 <- readSource("IRF", subtype = "bus-and-motor-coach-traffic")
    #million motor vehicles Km/yr
    a13 <- readSource("IRF", subtype = "total-four-wheeled-traffic")
    #million motor vehicles Km/yr
    a12 <- a12[, Reduce(intersect, list(getYears(a12), getYears(a13), getYears(x))), ]
    a13 <- a13[, Reduce(intersect, list(getYears(a12), getYears(a13), getYears(x))), ]
    x <- x[, Reduce(intersect, list(getYears(a12), getYears(a13), getYears(x))), ]
    
    out5 <- (a12 / a13)
    
    #bus-and-motor-coach-traffic / total-van,-pickup,-lorry-and-road-tractor-traffic
    x[, , "PB"][, , "GDO"] <- x[, , "PB"][, , "GDO"] * ifelse(is.na(out5), mean(out5, na.rm=TRUE), out5)
    x[, , "PB"][, , "GSL"] <- x[, , "PB"][, , "GSL"] * ifelse(is.na(out5), mean(out5, na.rm=TRUE), out5)
    
    #PN and GN
    a14 <- readSource("TREMOVE", subtype = "Stock")
    a14 <- a14[,,"REF"][,,"NAVIGATION"]
    PN <- dimSums(a14[,,"Passenger"],3)
    GN <- dimSums(a14[,,"Freight"],3)
    PN <- as.quitte(PN) %>%
      interpolate_missing_periods(period = getYears(PN, as.integer = TRUE)[1] : last(getYears(PN, as.integer = TRUE)), expand.values = TRUE)
    PN <- as.magpie(PN)
    GN <- as.quitte(GN) %>%
      interpolate_missing_periods(period = getYears(GN, as.integer = TRUE)[1] : last(getYears(GN, as.integer = TRUE)), expand.values = TRUE)
    GN <- as.magpie(GN)
    
    PN <- PN[, Reduce(intersect, list(getYears(PN), getYears(GN),getYears(x))), ]
    GN <- GN[, Reduce(intersect, list(getYears(PN), getYears(GN),getYears(x))), ]
    x <- x[, Reduce(intersect, list(getYears(PN), getYears(GN),getYears(x))), ]
    
    #million pKm/yr
    In_Nav <- PN + GN
    
    out6 <- (PN / In_Nav)
    out7 <- (GN / In_Nav)
    out7 <- toolCountryFill(out7, fill = NA)
    out6 <- toolCountryFill(out6, fill = NA)
    
    #Passenger inland navigation / inland navigation
    x[, , "GN"][, , "GDO"] <- x[, , "GN"][, , "GDO"] * ifelse(is.na(out7), mean(out7, na.rm=TRUE), out7)
    
    #Freight inland navigation / inland navigation
    x[, , "PN"][, , "GDO"] <- x[, , "PN"][, , "GDO"] * ifelse(is.na(out6), mean(out6, na.rm=TRUE), out6)
    
    l <- getNames(x) == "PA.KRS.Mt"
    getNames(x)[l] <- "PA.KRS.Mtoe"
    #from Mt to Mtoe
    x[,,"PA.KRS.Mtoe"] <- x[,,"PA.KRS.Mtoe"] / 1.027
    
    #add Hydrogen
    x <- add_columns(x, addnm = "H2F", dim = 3.2, fill = 10^-6)
  }

  #add Hydrogen
  a <- calcOutput(type = "HydrogenDemand", aggregate = FALSE)
  a <- a[,,getItems(a,3.1)[getItems(a,3.1) %in% sets]]
  
  if (subtype != "TRANSE") {
    x <- add_columns(x, addnm = "H2F", dim = 3.3, fill = 10^-6)
  }
  
  if (length(a) != 0) {
    a <- a[,getYears(x),]
    x <- as.quitte(x)
    a <- as.quitte(a)
    
    x <- full_join(a, x, by = c("model", "scenario", "region", "period", "variable", "unit", "new")) %>%
      mutate(value = ifelse(value.x == 10^-6 | is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
  }
  
  x <- as.magpie(x)
  
   # complete incomplete time series
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = getYears(x, as.integer = TRUE), expand.values = TRUE)
  # assign to countries with NA, their H12 region with weights
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")

  qx <- select(qx, -c("model", "scenario"))
  qx_bu <- qx

  ## assign to countries with NA, their H12 region with weights calculated from population

  population <- calcOutput(type = "POP", aggregate = FALSE)
  population <- as.quitte(population)

  # compute weights by population
  names(population) <- sub("region", "CountryCode", names(population))

  ## add mapping to population
  population <- left_join(population, h12, by = "CountryCode")
  value.x <- NULL
  value.y <- NULL
  weights <- NULL
  value <- NULL
  POP <- mutate(population, weights = sum(value, na.rm = TRUE), .by = c("RegionCode", "period"))
  POP["weights"] <- POP["value"] / POP["weights"]

  names(POP) <- sub("CountryCode", "region", names(POP))
  POP <- select(POP, -c("value", "model", "scenario", "X", "variable", "unit"))
  qx <- left_join(qx, POP, by = c("region", "period"))

  qx <- mutate(qx, value = sum(value, na.rm = TRUE), .by = c("RegionCode", "period", "new", "variable", "unit"))

  qx["value"] <- qx["value"] * qx["weights"]

  qx <- select(qx, -c("weights"))

  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "new", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y", "RegionCode"))

  ## assign to countries that still have NA, the global with weights
  qx_bu <- qx
  # compute weights by population
  POP <- mutate(population, weights = sum(value, na.rm = TRUE), .by = c("period"))
  POP["weights"] <- POP["value"] / POP["weights"]
  names(POP) <- sub("CountryCode", "region", names(POP))
  POP <- select(POP, -c("value", "model", "scenario", "X", "RegionCode", "variable", "unit"))
  qx <- left_join(qx, POP, by = c("region", "period"))

  qx <- mutate(qx, value = sum(value, na.rm = TRUE), .by = c("period", "new", "variable", "unit"))

  qx["value"] <- qx["value"] * qx["weights"]

  qx <- select(qx, -c("weights"))

  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "new", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  x <- as.quitte(qx) %>% as.magpie()
  
  if (subtype == "INDSE") {
    #OI is FE total per fuel - the sum of the other subsectors per fuel
    sum_subsectors <- dimSums(x[,,getItems(x,3.1)[!(getItems(x,3.1) %in% "OI")]][,,getItems(x[,,"OI"],3.3)], dim = 3.1, na.rm = TRUE)
    sum_subsectors <- as.quitte(sum_subsectors)
    sum_subsectors["variable"] <- "OI"
    sum_subsectors <- sum_subsectors[, c(1, 2, 3, 4, 8 , 5 , 6 , 7)]
    sum_subsectors <- as.quitte(sum_subsectors)
    sum_subsectors <- as.magpie(sum_subsectors)
    x[,,"OI"] <- x[,,"OI"] - sum_subsectors
    x[x < 0] <- 10^-6
    x[,,"OI"][,,"NGS"][x[,,"OI"][,,"NGS"] == 0] <- 10^-6
  }
  
  # set NA to 0
  x[is.na(x)] <- 10^-6
  
  i <- subtype
  Navigate <- calcOutput(type = "Navigate", subtype = i, aggregate = FALSE)
  Navigate <- as.quitte(Navigate)
    
  Primes <- calcOutput(type = "Primes", aggregate = FALSE)
  Primes <- as.quitte(Primes[,,intersect(getItems(Primes,3.1),sets)])
  
  #join Primes and Navigate
  Primes_Nav <- full_join(Primes, Navigate, by = c("model", "scenario", "region", "period", "variable", "unit", "new")) %>%
    mutate(value = ifelse(value.x == 10^-6 | is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  Primes_Nav <- as.magpie(Primes_Nav)
  Primes_Nav[is.na(Primes_Nav)] <- 10^-6
  Primes_Nav <- as.quitte(Primes_Nav)
  z <- Primes_Nav
  
  #join ENERDATA_IEA and Primes_Nav
  qx <- full_join(as.quitte(x), z, by = c("model", "scenario", "region", "period", "variable", "unit", "new")) %>%
    mutate(value = ifelse(value.x == 0 | is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))

  x <- as.quitte(qx) %>% as.magpie()
  # set NA to 0
  x[is.na(x)] <- 10^-6
  x <- x[,fStartHorizon : gsub("y","",last(getYears(x))),]
  
  #extrapolate_ENERDATA_IEA_if_there_are_not_data_from_Navigate
  
  extrapolate <- x[,fStartHorizon : 2023,]
  
  if (subtype == "TRANSE") {
    extrapolate <- x[,2015:2020,]
  }
  
  extrapolate_x <- as.quitte(extrapolate) %>%
    interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
  
  qextrapolate_x <- full_join(as.quitte(x), extrapolate_x, by = c("model", "scenario", "region", "period", "variable", "unit", "new")) %>%
    mutate(value = ifelse(value.x == 10^-6, value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  
  x <- as.quitte(qextrapolate_x) %>% as.magpie()
  
  # set NA to 0
  x[is.na(x)] <- 10^-6
  x[isZero(x)] <- 10^-6
  
  if (subtype == "TRANSE") {
    x["MLT",,"PT"] <- 10^-6
    x["CYP",,"PT"] <- 10^-6
  }
  
  list(x = x,
       weight = NULL,
       unit = "various",
       description = "ENERDATA, IEA, TREMOVE and NAVIGATE; fuel consumption in XXX sector")

}
