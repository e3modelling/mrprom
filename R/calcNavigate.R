#' calcNavigate
#'
#' Use Navigate and calcIFuelCons fuel consumption in XXX sector.
#' The data for the years 2010 : 2021 is from calcIFuelCons.
#' For TRANSE is until year 2020 from calcIFuelCons.
#' The data for the years 2022 : 2100 is from Navigate.
#' (XXX: NENSE, INDSE, DOMSE, TRANSE).
#'
#' @param subtype string sector (DOMSE, INDSE, NENSE, TRANSE)
#' @return Navigate and calcIFuelCons fuel consumption in XXX sector
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "Navigate", subtype = "DOMSE", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select full_join
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom utils tail


calcNavigate <- function(subtype = "DOMSE") {
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  # load current OPENPROM set configuration
  sets <- toolGetMapping(paste0(subtype, ".csv"),
                         type = "blabla_export",
                         where = "mrprom")
  
  sets <- as.character(sets[, 1])
  
  # use navigate-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-navigate-fucon-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")
  maps <- map
  
  ## filter mapping to keep only XXX sectors
  map <- filter(map, map[, "SBS"] %in% sets)
  ## ..and only items that have an Navigate-prom mapping
  Navigate <- map[!is.na(map[, "Navigate"]), "Navigate"]
  map <- map[map[, "Navigate"] %in% Navigate, ]
  #remove the empty cells from mapping
  map <- map[!(map[, "Navigate"] == ""), ]
  
  #filter navigate data by scenario different for each sector
  if (subtype %in% c("DOMSE", "NENSE")) {
    x1 <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = TRUE)
    
    x1 <- x1[,,map[map[,"Navigate"] %in% getItems(x1,3.3), 6]]
    
    x1 <- as.quitte(x1) %>%
      interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
    
    x1 <- as.quitte(x1) %>% as.magpie()
    
    x2 <- readSource("Navigate", subtype = "NAV_Dem-NPi-ref", convert = TRUE)
    
    x2 <- x2[,,map[map[,"Navigate"] %in% getItems(x2,3.3), 6]]
    
    x2 <- as.quitte(x2) %>%
      interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
    
    x2 <- as.quitte(x2) %>% as.magpie()
    
    #keep common years that exist in the scenarios
    years <- intersect(getYears(x1,as.integer=TRUE),getYears(x2,as.integer=TRUE))
    x <- mbind(x1[, years,], x2[, years,])
  }
  #for TRANSE use of NAV_Ind_NPi because it has truck data
  if (subtype %in% c("INDSE", "TRANSE")) {
    x1 <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = TRUE)
    
    x1 <- x1[,,map[map[,"Navigate"] %in% getItems(x1,3.3), 6]]
    
    x1 <- as.quitte(x1) %>%
      interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
    
    x1 <- as.quitte(x1) %>% as.magpie()
    
    x2 <- readSource("Navigate", subtype = "NAV_Ind_NPi", convert = TRUE)
    
    x2 <- x2[,,map[map[,"Navigate"] %in% getItems(x2,3.3), 6]]
    
    x2 <- as.quitte(x2) %>%
      interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
    
    x2 <- as.quitte(x2) %>% as.magpie()
    
    #keep common years that exist in the scenarios
    years <- intersect(getYears(x1,as.integer=TRUE),getYears(x2,as.integer=TRUE))
    x <- mbind(x1[, years,], x2[, years,])
  }
  
  # filter data to keep only Navigate variables
  x <- x[, , map[, "Navigate"]]
  #EJ to Mtoe
  x <- x * 23.8846
  getItems(x, 3.4) <- "Mtoe"
  
  x <- as.quitte(x)
  value.x <- NULL
  value.y <- NULL
  value <- NULL
  
  #if SUP_NPi_Default has NA take the value of the second scenario
  x <- full_join(x[which(x[,2] == "SUP_NPi_Default"),], x[which(x[,2] != "SUP_NPi_Default"),], by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y", "scenario"))
  
  
  #take the mean value from the available models
  x <- mutate(x, value = mean(value, na.rm = TRUE), .by = c("region", "period", "variable", "unit"))
  #drop column model
  x <- x %>% select(-c("model"))
  #remove duplicates from data 
  x <- distinct(x)
  
  #rename variables from Navigate to openprom names
  names(map) <- gsub("Navigate", "variable", names(map))
  x <- left_join(x, map[,  c(2,3,6)], by = "variable")
  
  #drop variable names of navigate
  x <- x[,c(1, 3, 4, 5, 6, 7)]
  
  #rename columns of data
  names(x) <- gsub("SBS", "variable", names(x))
  names(x) <- gsub("EF", "new", names(x))
  
  x <- as.quitte(x) %>% as.magpie()
  
  #add dimensions, GDO 75% of LQD and GSL 25% of LQD
  x <- add_columns(x, addnm = c("GDO"), dim = "new", fill = 0.75)
  y <- add_columns(x, addnm = c("LQD"), dim = "new", fill = 0)
  y <- y[,,getItems(y[,,"LQD"],3)[!(getItems(y[,,"LQD"],3) %in% getItems(x[,,"LQD"],3))]]
  x <- mbind(x, y)
  x[,,"GDO"] <- x[,,"LQD"] * x[,,"GDO"]
  
  x <- add_columns(x, addnm = c("GSL"), dim = "new", fill = 0.25)
  y2 <- add_columns(x, addnm = c("LQD"), dim = "new", fill = 0)
  y2 <- y2[,,getItems(y2[,,"LQD"],3)[!(getItems(y2[,,"LQD"],3) %in% getItems(x[,,"LQD"],3))]]
  x <- mbind(x, y2)
  x[,,"GSL"] <- x[,,"LQD"] * x[,,"GSL"]
  
  if (subtype == "INDSE") {
    #OI is FE total per fuel - the sum of the other subsectors per fuel
    sum_subsectors <- dimSums(x[,,getItems(x,3.1)[!(getItems(x,3.1) %in% "OI")]][,,getItems(x[,,"OI"],3.2)], dim = 3.1, na.rm = TRUE)
    sum_subsectors <- as.quitte(sum_subsectors)
    sum_subsectors["variable"] <- "OI"
    sum_subsectors <- sum_subsectors[, c(1, 2, 3, 4, 8 , 5 , 6 , 7)]
    sum_subsectors <- as.quitte(sum_subsectors)
    sum_subsectors <- as.magpie(sum_subsectors)
    x[,,"OI"] <- x[,,"OI"] - ifelse(is.na(sum_subsectors), 0, sum_subsectors)
    x[x < 0] <- 10^-6
    x[,,"OI"][,,"NGS"][x[,,"OI"][,,"NGS"] == 0] <- 10^-6
  }
  
  if (subtype == "TRANSE") {
    
    a1 <- readSource("IRF", subtype = "inland-surface-passenger-transport-by-rail")
    #million pKm/yr
    a2 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-rail")
    #million tKm/yr
    a1 <- a1[, Reduce(intersect, list(getYears(a1), getYears(a2))), ]
    a2 <- a2[, Reduce(intersect, list(getYears(a1), getYears(a2))), ]
    out1 <- (a1 / (a1 + a2))
    out1 <- ifelse(is.na(out1), mean(out1, na.rm=TRUE), out1)
    out1 <- as.quitte(out1)
    out1 <- mutate(out1, value = mean(value, na.rm = TRUE), .by = c("region"))
    out1 <- select(out1, c("region", "value"))
    out1 <- distinct(out1)
    out1 <- as.quitte(out1) %>% as.magpie()
    x[,,"PT"] <- x[,,"PT"] * out1
    
    out3 <- (a2 / (a1 + a2))
    out3 <- ifelse(is.na(out3), mean(out3, na.rm=TRUE), out3)
    out3 <- as.quitte(out3)
    out3 <- mutate(out3, value = mean(value, na.rm = TRUE), .by = c("region"))
    out3 <- select(out3, c("region", "value"))
    out3 <- distinct(out3)
    out3 <- as.quitte(out3) %>% as.magpie()
    x[,,"GT"] <- x[,,"GT"] * out3
    
    a3 <- readSource("IRF", subtype = "inland-surface-private-passenger-transport-by-road")
    #million pKm/yr
    a4 <- readSource("IRF", subtype = "inland-surface-passenger-transport-total")
    #million pKm/yr
    a3 <- a3[, Reduce(intersect, list(getYears(a3), getYears(a4))), ]
    a4 <- a4[, Reduce(intersect, list(getYears(a3), getYears(a4))), ]
    out2 <- (a3 / (a4))
    out2 <- ifelse(is.na(out2), mean(out2, na.rm=TRUE), out2)
    out2 <- as.quitte(out2)
    out2 <- mutate(out2, value = mean(value, na.rm = TRUE), .by = c("region"))
    out2 <- select(out2, c("region", "value"))
    out2 <- distinct(out2)
    out2 <- as.quitte(out2) %>% as.magpie()
    x[,,"PC"] <- x[,,"PC"] * out2
    
    a5 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-inland-waterway")
    #million tKm/yr
    a6 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-rail")
    #million tKm/yr
    a7 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-road")
    #million tKm/yr

    a5 <- a5[, Reduce(intersect, list(getYears(a5), getYears(a6), getYears(a7))), ]
    a6 <- a6[, Reduce(intersect, list(getYears(a5), getYears(a6), getYears(a7))), ]
    a7 <- a7[, Reduce(intersect, list(getYears(a5), getYears(a6), getYears(a7))), ]
    
    out4 <- (a5 / (a5 + a6 + a7))
    out4 <- ifelse(is.na(out4), mean(out4, na.rm=TRUE), out4)
    out4 <- as.quitte(out4)
    out4 <- mutate(out4, value = mean(value, na.rm = TRUE), .by = c("region"))
    out4 <- select(out4, c("region", "value"))
    out4 <- distinct(out4)
    out4 <- as.quitte(out4) %>% as.magpie()
    x[,,"GN"] <- x[,,"GN"] * out4
    
    # remove GSL from PT & GT & GU in iFuelConsTRANSE
    x[,,"PT"][,,"GSL"] <- 10^-6
    x[,,"GT"][,,"GSL"] <- 10^-6
    x[,,"GU"][,,"GSL"] <- 10^-6
    
    # We want 100% of liquids to be GDO for GT & PT & GU
    
    x[,,"PT"][,,"GDO"] <- x[,,"PT"][,,"GDO"] / 0.75
    x[,,"GT"][,,"GDO"] <- x[,,"GT"][,,"GDO"] / 0.75
    x[,,"GU"][,,"GDO"] <- x[,,"GU"][,,"GDO"] / 0.75
    
  }
  
  # complete incomplete time series
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
  
  if (subtype == "TRANSE") {
    
    #TREMOVE data
    a <- readSource("TREMOVE", subtype = "FuelOutlook")
    
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
    
    a <- a[,,"REF"]
    
    a <- as.quitte(a)
    
    names(a) <- sub("variable", "new", names(a))
    names(a) <- sub("technology", "variable", names(a))
    
    a <- select(a, -("sector"))
    
    a[,"scenario"] <- "(Missing)"
    
    a <-  as.quitte(a) %>%
      interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
    
    #join TREMOVE and Navigate
    qx <- full_join(a, qx, by = c("model", "scenario", "region", "period", "variable", "unit", "new")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
  }
  
  
  i <- subtype
  a <- calcOutput(type = "IFuelCons", subtype = i, aggregate = FALSE)
  IFuelCons <- as.quitte(a)
  
  
  #join calcIFuelCons and Navigate
  qx <- full_join(IFuelCons, qx, by = c("model", "scenario", "region", "period", "variable", "unit", "new")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  
  # 
  # # assign to countries with NA, their H12 region with weights
  # h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  # 
  # qx <- select(qx, -c("model", "scenario"))
  # qx_bu <- qx
  # 
  # ## assign to countries with NA, their H12 region with weights calculated from population
  # 
  # population <- calcOutput(type = "POP", aggregate = FALSE)
  # population <- as.quitte(population)
  # 
  # # compute weights by population
  # names(population) <- sub("region", "CountryCode", names(population))
  # 
  # ## add mapping to population
  # population <- left_join(population, h12, by = "CountryCode")
  # value.x <- NULL
  # value.y <- NULL
  # weights <- NULL
  # value <- NULL
  # POP <- mutate(population, weights = sum(value, na.rm = TRUE), .by = c("RegionCode", "period"))
  # POP["weights"] <- POP["value"] / POP["weights"]
  # 
  # names(POP) <- sub("CountryCode", "region", names(POP))
  # POP <- select(POP, -c("value", "model", "scenario", "X", "data", "variable", "unit"))
  # qx <- left_join(qx, POP, by = c("region", "period"))
  # 
  # qx <- mutate(qx, value = sum(value, na.rm = TRUE), .by = c("RegionCode", "period", "new", "variable", "unit"))
  # 
  # qx["value"] <- qx["value"] * qx["weights"]
  # 
  # qx <- select(qx, -c("weights"))
  # 
  # qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "new", "unit")) %>%
  #   mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
  #   select(-c("value.x", "value.y", "RegionCode"))
  # 
  # ## assign to countries that still have NA, the global with weights
  # qx_bu <- qx
  # # compute weights by population
  # POP <- mutate(population, weights = sum(value, na.rm = TRUE), .by = c("period"))
  # POP["weights"] <- POP["value"] / POP["weights"]
  # names(POP) <- sub("CountryCode", "region", names(POP))
  # POP <- select(POP, -c("value", "model", "scenario", "X", "RegionCode", "data", "variable", "unit"))
  # qx <- left_join(qx, POP, by = c("region", "period"))
  # 
  # qx <- mutate(qx, value = sum(value, na.rm = TRUE), .by = c("period", "new", "variable", "unit"))
  # 
  # qx["value"] <- qx["value"] * qx["weights"]
  # 
  # qx <- select(qx, -c("weights"))
  # 
  # qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "new", "unit")) %>%
  #   mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
  #   select(-c("value.x", "value.y"))
  x <- as.quitte(qx) %>% as.magpie()
  # set NA to 0
  x[is.na(x)] <- 10^-6
  x <- x[,fStartHorizon : 2100,]
  
  #extrapolate_FuelCons_if_there_are_not_data_from_Navigate
  
  extrapolate <- x[,2010:2021,]
  
  if (subtype == "TRANSE") {
    extrapolate <- x[,2015:2020,]
  }
  
  extrapolate_x <- as.quitte(extrapolate) %>%
    interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
  
  qextrapolate_x <- full_join(as.quitte(x), extrapolate_x, by = c("model", "scenario", "region", "period", "variable", "unit", "new")) %>%
    mutate(value = ifelse(value.x == 10^-6, value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  
  x <- as.quitte(qextrapolate_x) %>% as.magpie()
  
  list(x = x,
       weight = NULL,
       unit = "Mtoe",
       description = "Navigate and calcIFuelCons fuel consumption in XXX sector")
  
}
