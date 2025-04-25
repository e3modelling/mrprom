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

  # load data source (ENERDATA)
  x <- readSource("ENERDATA", "consumption", convert = TRUE)

  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  lastYear <- sub("y", "", tail(sort(getYears(x)), 1))
  x <- x[, c(fStartHorizon:lastYear), ]

  # load current OPENPROM set configuration
  sets <- toolGetMapping(paste0(subtype, ".csv"),
                         type = "blabla_export",
                         where = "mrprom")
  
  sets <- as.character(sets[, 1])

  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-enerdata-fucon-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")
  maps <- map
  ## filter mapping to keep only XXX sectors
  map <- filter(map, map[, "SBS"] %in% sets)
  ## ..and only items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  map <- map[map[, "ENERDATA"] %in% enernames, ]
  ## filter data to keep only XXX data
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  x <- x[, , enernames]
  ## for oil, rename unit from Mt to Mtoe
  if (any(grepl("oil", getItems(x, 3.1)) & grepl("Mt$", getNames(x)))) {
    tmp <- x[, , "Mt"]
    getItems(tmp, 3.2) <- "Mtoe"
    x <- mbind(x[, , "Mtoe"], tmp)
    map[["ENERDATA"]] <-  sub(".Mt$", ".Mtoe", map[["ENERDATA"]])
  }

  ## rename variables to openprom names
  out <- NULL
  ## rename variables from ENERDATA to openprom names
  ff <- paste(map[, 2], map[, 3], sep = ".")
  iii <- 0
  ### add a dummy dimension to data because mapping has 3 dimensions, and data only 2
  for (ii in map[, "ENERDATA"]) {
    iii <- iii + 1
    out <- mbind(out, setNames(add_dimension(x[, , ii], dim = 3.2), paste0(ff[iii], ".", sub("^.*.\\.", "", getNames(x[, , ii])))))
  }
  x <- out
  
  IEA <- NULL
  q <- as.quitte(x)
  map <- map %>% drop_na(IEA)
  
  #the map has a column SBS which corresponds to flow of IEA
  for (ii in unique(map[, "flow"])) {
    d <- readSource("IEA", subtype = as.character(ii))
    d <- d / 1000 #ktoe to mtoe
    d <- as.quitte(d)
    #each flow has some products as it is the EF column of map
    m <- filter(map, map[["flow"]] == ii)
    #for each product of IEA data
    region <- NULL
    period <- NULL
    for (i in 1 : nrow(m)) {
      #filter the IEA data (of a specific flow) with product
      qb <- filter(d, d[["product"]] == m[i, 4])
      qb <- select((qb), c(region, period, value))
      #flow MARBUNK has negative values
      if (ii == "MARBUNK") {
        qb["value"] <- - qb["value"]
      }
      #join ENERDATA and IEA
      q <- left_join(q, qb, by = c("region", "period"))
      #if ENERDATA is na take the value of IEA
      q[which(q[, 8] == m[i, 3] & q[, 4] == m[i, 2]), ] <- q[which(q[, 8] == m[i, 3] & q[, 4] == m[i, 2]), ] %>% mutate(`value.x` = ifelse(is.na(`value.x`), `value.y`, `value.x`))
      q[which(q[, 8] == m[i, 3] & q[, 4] == m[i, 2]), ] <- q[which(q[, 8] == m[i, 3] & q[, 4] == m[i, 2]), ] %>% mutate(`value.x` = ifelse((isZero(`value.x`) & !isZero(`value.y`)& !is.na(`value.y`)), `value.y`, `value.x`))
      names(q) <- sub("value.x", "value", names(q))
      q <- select((q), -c(`value.y`))
    }
    #add data of IEA which ENERDATA does not have
    #remove rows from map that compared with ENERDATA in the previous step
    l <- !(do.call(paste0, maps) %in% do.call(paste0, map))
    map2 <- maps[l, ]
    map2 <- map2 %>% drop_na(IEA)
    #for a specific flow take the products
    mapping <- map2[which(map2["flow"] == ii), 4]
    ##filter the IEA data (of a specific flow) with products
    qy <- filter(d, d[["product"]] %in% mapping)
    if (ii == "MARBUNK") {
      qy["value"] <- - qy["value"]
    }
    names(map2) <- sub("IEA", "product", names(map2))
    #rename from IEA names to OPEN PROM names
    qy <- left_join(qy, map2, by = c("product", "flow"))
    qy["variable"] <- qy["SBS"]
    qy["new"] <- qy["EF"]
    qy["unit"] <- "Mtoe"
    qy <- select(qy, -c("SBS", "EF", "ENERDATA", "product", "flow"))
    #ENERDATA has values 2010:2021 
    qy <- filter(qy, qy[["period"]] %in% fStartHorizon : tail(getYears(x, as.integer = TRUE), n = 1))
    q <- rbind(q, qy)
  }
  
  if (subtype == "TRANSE") {
    #variable, new , unit
    q <- q[, c(1, 2, 3, 4, 8 , 5 , 6 , 7)]
  }
  
  x <- as.magpie(q)
  
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
    x[, , "PT.GDO.Mtoe"] <- x[, , "PT.GDO.Mtoe"] * ifelse(is.na(out3), mean(out3, na.rm=TRUE), out3)
    #inland-surface-freight-transport-by-rail / total inland-surface
    x[, , "GT.GDO.Mtoe"] <- x[, , "GT.GDO.Mtoe"] * ifelse(is.na(out4), mean(out4, na.rm=TRUE), out4)

    x[, , "PT.ELC.Mtoe"] <- x[, , "PT.ELC.Mtoe"] * ifelse(is.na(out3), mean(out3, na.rm=TRUE), out3)
    x[, , "GT.ELC.Mtoe"] <- x[, , "GT.ELC.Mtoe"] * ifelse(is.na(out4), mean(out4, na.rm=TRUE), out4)

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
    x[, , "PC.GDO.Mtoe"] <- x[, , "PC.GDO.Mtoe"] * ifelse(is.na(out1), mean(out1, na.rm=TRUE), out1)
    x[, , "PC.GSL.Mtoe"] <- x[, , "PC.GSL.Mtoe"] * ifelse(is.na(out1), mean(out1, na.rm=TRUE), out1)
    x[, , "GU.GDO.Mtoe"] <- x[, , "GU.GDO.Mtoe"] * ifelse(is.na(out2), mean(out2, na.rm=TRUE), out2)
    
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
    x[, , "PB.GDO.Mtoe"] <- x[, , "PB.GDO.Mtoe"] * ifelse(is.na(out5), mean(out5, na.rm=TRUE), out5)
    x[, , "PB.GSL.Mtoe"] <- x[, , "PB.GSL.Mtoe"] * ifelse(is.na(out5), mean(out5, na.rm=TRUE), out5)
    
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
    x[, , "GN.GDO.Mtoe"] <- x[, , "GN.GDO.Mtoe"] * ifelse(is.na(out7), mean(out7, na.rm=TRUE), out7)
    x[, , "GN.HCL.Mtoe"] <- x[, , "GN.HCL.Mtoe"] * ifelse(is.na(out7), mean(out7, na.rm=TRUE), out7)
    
    #Freight inland navigation / inland navigation
    x[, , "PN.GDO.Mtoe"] <- x[, , "PN.GDO.Mtoe"] * ifelse(is.na(out6), mean(out6, na.rm=TRUE), out6)
    x[, , "PN.HCL.Mtoe"] <- x[, , "PN.HCL.Mtoe"] * ifelse(is.na(out6), mean(out6, na.rm=TRUE), out6)
    
    l <- getNames(x) == "PA.KRS.Mt"
    getNames(x)[l] <- "PA.KRS.Mtoe"
    #from Mt to Mtoe
    x[,,"PA.KRS.Mtoe"] <- x[,,"PA.KRS.Mtoe"] / 1.027
  }

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
  
  z <- Navigate
  
  if (subtype == "TRANSE") {
    
    TREMOVE <- calcOutput(type = "TREMOVE", aggregate = FALSE)
    TREMOVE <- as.quitte(TREMOVE)
    
    #join TREMOVE and Navigate
    Trem_Nav <- full_join(TREMOVE, Navigate, by = c("model", "scenario", "region", "period", "variable", "unit", "new")) %>%
      mutate(value = ifelse(value.x == 10^-6 | is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    Trem_Nav <- as.magpie(Trem_Nav)
    Trem_Nav[is.na(Trem_Nav)] <- 10^-6
    Trem_Nav <- as.quitte(Trem_Nav)
    z <- Trem_Nav
  }
  
  #join ENERDATA_IEA and Trem_Nav
  qx <- full_join(as.quitte(x), z, by = c("model", "scenario", "region", "period", "variable", "unit", "new")) %>%
    mutate(value = ifelse(value.x == 0 | is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))

  x <- as.quitte(qx) %>% as.magpie()
  # set NA to 0
  x[is.na(x)] <- 10^-6
  x <- x[,fStartHorizon : gsub("y","",last(getYears(x))),]
  
  #extrapolate_ENERDATA_IEA_if_there_are_not_data_from_Navigate
  
  extrapolate <- x[,fStartHorizon : 2021,]
  
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
  
  list(x = x,
       weight = NULL,
       unit = "various",
       description = "ENERDATA, IEA, TREMOVE and NAVIGATE; fuel consumption in XXX sector")

}
