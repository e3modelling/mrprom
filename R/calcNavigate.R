#' calcNavigate
#'
#' Use Navigate and calcIFuelCons fuel consumption in XXX sector.
#' The data for the years 2020 : 2021 is from calcIFuelCons.
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
#' @importFrom dplyr filter %>% mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom utils tail
#' @importFrom stringr str_replace_all


calcNavigate <- function(subtype = "DOMSE") {
  
  # load current OPENPROM set configuration
  sets <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), subtype)
  sets <- unlist(strsplit(sets[, 1], ","))
  
  # use navigate-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-navigate-fucon-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")
  maps <- map
  #remove unwanted symbols from map 
  map[["Navigate"]] <- str_replace_all(map[["Navigate"]], "[^[:alnum:]]", " ")

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
  #remove unwanted symbols from data 
  getItems(x,3.3) <- str_replace_all(getItems(x,3.3), "[^[:alnum:]]", " ")
  
  # filter data to keep only Navigate variables
  x <- x[, , map[, "Navigate"]]
  #EJ to Mtoe
  x <- x * 23.8846
  getItems(x, 3.4) <- "Mtoe"
  
  x <- as.quitte(x)
  
  value <- NULL
  #take the mean value from the available models and scenarios
  x <- mutate(x, value = mean(value, na.rm = TRUE), .by = c("region", "period", "variable", "unit"))
  #drop column model,scenario
  x <- x[, c(3 : 7)]
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
  # complete incomplete time series
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = 2010 : 2100, expand.values = TRUE)
  
  a <- calcOutput(type = "IFuelCons", subtype, aggregate = FALSE)
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
  
  list(x = x,
       weight = NULL,
       unit = "Mtoe",
       description = "Navigate and calcIFuelCons fuel consumption in XXX sector")
  
}
