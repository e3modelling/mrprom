#' calcNavigate
#'
#' Use Navigate consumption data to derive parameter iFuelConsXXX
#' (XXX: NENSE, INDSE, DOMSE, TRANSE).
#'
#' @param subtype string, OPENPROM sector (DOMSE, INDSE, NENSE, TRANSE)
#' @return  OPENPROM input data iFuelConsXXX
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
  
  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-navigate-fucon-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")
  maps <- map
  map[["Navigate"]] <- str_replace_all(map[["Navigate"]], "[^[:alnum:]]", " ")
  map <- map[, c(1:6)]
  
  
  ## filter mapping to keep only XXX sectors
  map <- filter(map, map[, "SBS"] %in% sets)
  ## ..and only items that have an enerdata-prom mapping
  Navigate <- map[!is.na(map[, "Navigate"]), "Navigate"]
  map <- map[map[, "Navigate"] %in% Navigate, ]
  map <- map[!(map[, "Navigate"] == ""), ]
  
  x <- readSource("NavigateConsumption", convert = TRUE)
  getItems(x,3.3) <- str_replace_all(getItems(x,3.3), "[^[:alnum:]]", " ")
    
  x <- x[, , map[, 6]]
  x <- x * 23.8846
  getItems(x, 3.4) <- "Mtoe"
  
  if (subtype %in% c("DOMSE", "NENSE", "TRANSE")) {
    x <- x[, , "NAV_Dem-NPi-ref"]
  }
  
  if (subtype %in% c("INDSE")) {
    x <- x[, , "NAV_Ind_NPi"]
  }
  
  x <- as.quitte(x)
  
  value <- NULL
  x <- mutate(x, value = mean(value, na.rm = TRUE), .by = c("region", "period", "scenario", "variable", "unit"))
  x <- x[, c(2 : 7)]
  x <- distinct(x)
  names(map) <- gsub("Navigate", "variable", names(map))
  x <- left_join(x, map[,  c(2,3,6)], by = "variable")
  x <- x[,c(1, 2, 4, 5, 6, 7, 8)]
  names(x) <- gsub("SBS", "variable", names(x))
  names(x) <- gsub("EF", "new", names(x))
  
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
  POP <- select(POP, -c("value", "model", "scenario", "X", "data", "variable", "unit"))
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
  POP <- select(POP, -c("value", "model", "scenario", "X", "RegionCode", "data", "variable", "unit"))
  qx <- left_join(qx, POP, by = c("region", "period"))
  
  qx <- mutate(qx, value = sum(value, na.rm = TRUE), .by = c("period", "new", "variable", "unit"))
  
  qx["value"] <- qx["value"] * qx["weights"]
  
  qx <- select(qx, -c("weights"))
  
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "new", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  x <- as.quitte(qx) %>% as.magpie()
  # set NA to 0
  x[is.na(x)] <- 10^-6
  x <- toolCountryFill(x, fill = NA)
  x <- x[,2010:2100,]
  
  list(x = x,
       weight = NULL,
       unit = "various",
       description = "fuel consumption in XXX sector")
  
}
