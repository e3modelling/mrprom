#' calcNavigate
#'
#' Use Navigate fuel consumption in XXX sector.
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
  
  if (subtype == "TRANSE") {
    sets <- c(sets, "PB", "PN")
  }
  
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
    
    x1 <- x1[,,unique(map[map[,"Navigate"] %in% getItems(x1,3.3), 6])]
    
    x1 <- as.quitte(x1) %>%
      interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
    
    x1 <- as.quitte(x1) %>% as.magpie()
    
    x2 <- readSource("Navigate", subtype = "NAV_Dem-NPi-ref", convert = TRUE)
    
    x2 <- x2[,,unique(map[map[,"Navigate"] %in% getItems(x2,3.3), 6])]
    
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
    
    x1 <- x1[,,unique(map[map[,"Navigate"] %in% getItems(x1,3.3), 6])]
    
    x1 <- as.quitte(x1) %>%
      interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
    
    x1 <- as.quitte(x1) %>% as.magpie()
    
    x2 <- readSource("Navigate", subtype = "NAV_Ind_NPi", convert = TRUE)

    x2 <- x2[,,unique(map[map[,"Navigate"] %in% getItems(x2,3.3), 6])]
    
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
    
    #PB
    a8 <- readSource("IRF", subtype = "inland-surface-public-passenger-transport-by-road")
    #million pKm/yr
    a8 <- a8[, Reduce(intersect, list(getYears(a8), getYears(a4))), ]
    a4 <- a4[, Reduce(intersect, list(getYears(a8), getYears(a4))), ]
    out5 <- (a8 / (a4))
    out5 <- ifelse(is.na(out5), mean(out5, na.rm=TRUE), out5)
    out5 <- as.quitte(out5)
    out5 <- mutate(out5, value = mean(value, na.rm = TRUE), .by = c("region"))
    out5 <- select(out5, c("region", "value"))
    out5 <- distinct(out5)
    out5 <- as.quitte(out5) %>% as.magpie()
    x[,,"PB"] <- x[,,"PB"] * out5
    
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
  
  x <- as.quitte(qx) %>% as.magpie()
  # set NA to 0
  x[is.na(x)] <- 10^-6
  x <- x[,fStartHorizon : 2100,]
  
  list(x = x,
       weight = NULL,
       unit = "Mtoe",
       description = "Navigate fuel consumption in XXX sector")
  
}
