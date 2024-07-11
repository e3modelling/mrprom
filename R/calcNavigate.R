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
  sets <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), subtype)
  sets <- unlist(strsplit(sets[, 1], ","))
  
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
    x2 <- readSource("Navigate", subtype = "NAV_Dem-NPi-ref", convert = TRUE)
    #keep common years that exist in the scenarios
    years <- intersect(getYears(x1,as.integer=TRUE),getYears(x2,as.integer=TRUE))
    x <- mbind(x1[, years,], x2[, years,])
  }
  #for TRANSE use of NAV_Ind_NPi because it has truck data
  if (subtype %in% c("INDSE", "TRANSE")) {
    x1 <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = TRUE)
    x2 <- readSource("Navigate", subtype = "NAV_Ind_NPi", convert = TRUE)
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
  
  if (subtype == "TRANSE") {
    
    a1 <- readSource("IRF", subtype = "inland-surface-passenger-transport-by-rail")
    #million pKm/yr
    a2 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-rail")
    #million tKm/yr
    a1 <- a1[, Reduce(intersect, list(getYears(a1), getYears(a2))), ]
    a2 <- a2[, Reduce(intersect, list(getYears(a1), getYears(a2))), ]
    out1 <- (a1 / (a1 + a2))
    out1 <- ifelse(is.na(out1), 1, out1)
    out1 <- as.quitte(out1)
    out1 <- mutate(out1, value = mean(value, na.rm = TRUE), .by = c("region"))
    out1 <- select(out1, c("region", "value"))
    out1 <- distinct(out1)
    out1 <- as.quitte(out1) %>% as.magpie()
    x[,,"PT"] <- x[,,"PT"] * out1
    
    out3 <- (a2 / (a1 + a2))
    out3 <- ifelse(is.na(out3), 1, out3)
    out3 <- as.quitte(out3)
    out3 <- mutate(out3, value = mean(value, na.rm = TRUE), .by = c("region"))
    out3 <- select(out3, c("region", "value"))
    out3 <- distinct(out3)
    out3 <- as.quitte(out3) %>% as.magpie()
    x[,,"GT"] <- x[,,"GT"] * out3
    
    a3 <- readSource("IRF", subtype = "inland-surface-public-passenger-transport-by-road")
    #million pKm/yr
    a4 <- readSource("IRF", subtype = "inland-surface-passenger-transport-by-rail")
    #million tKm/yr
    a3 <- a3[, Reduce(intersect, list(getYears(a3), getYears(a4))), ]
    a4 <- a4[, Reduce(intersect, list(getYears(a3), getYears(a4))), ]
    out2 <- (a3 / (a3 + a4))
    out2 <- ifelse(is.na(out2), 1, out2)
    out2 <- as.quitte(out2)
    out2 <- mutate(out2, value = mean(value, na.rm = TRUE), .by = c("region"))
    out2 <- select(out2, c("region", "value"))
    out2 <- distinct(out2)
    out2 <- as.quitte(out2) %>% as.magpie()
    x[,,"PC"] <- x[,,"PC"] * out2
    
  }
  
  # complete incomplete time series
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
  
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
  
  list(x = x,
       weight = NULL,
       unit = "Mtoe",
       description = "Navigate and calcIFuelCons fuel consumption in XXX sector")
  
}
