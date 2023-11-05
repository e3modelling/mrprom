#' calcIDataElecSteamGen
#'
#' Use data to derive OPENPROM input parameter iDataElecSteamGen
#'
#' @return  OPENPROM input data iDataElecSteamGen
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataElecSteamGen", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte

calcIDataElecSteamGen <- function() {
  
  x <- readSource("ENERDATA", "capacity", convert = TRUE)
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fStartY <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartY"]
  x <- x[, c(fStartHorizon:fStartY), ]
  
  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-enerdata-pgall-mapping.csv",
                        type = "sectoral",
                        where = "mappingfolder")
  
  ## ..and only items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA..MW."]), "ENERDATA..MW."])
  map <- map[map[, "ENERDATA..MW."] %in% enernames, ]

  enernames <- unique(map[!is.na(map[, "ENERDATA..MW."]), "ENERDATA..MW."])
  enernames <- enernames[! enernames %in% c("")]
  enernames[2] <- "Total electricity capacity coal, lignite (multifuel included)"
  enernames[4] <- "Total electricity capacity gas (multifuel oil/gas included)"
  x <- x[, , enernames]
  x[, ,enernames[2]] <- x[, , enernames[2]] - x[, ,enernames[1]]
  x[, ,enernames[4]] <- x[, , enernames[4]] - x[, ,enernames[6]]
  
  getNames(x)[2] <- "Total electricity capacity coal, lignite (multifuel included).MW - Single fired electricity capacity lignite.MW"
  getNames(x)[4] <- "Total electricity capacity gas (multifuel oil/gas included).MW - Installed capacity in combined cycles.MW"
  ## rename variables from ENERDATA to openprom names
  ff <- map[!(map[,2]==""), 1]
  getNames(x) <- ff
  
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = getYears(x, as.integer = TRUE), expand.values = TRUE)  
  qx_bu<- qx
  # assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv")
  names(qx) <- sub("region", "CountryCode", names(qx))
  ## add h12 mapping to dataset
  qx <- left_join(qx, h12, by="CountryCode")
  ## add new column containing regional mean value
  value <- NULL
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "variable"))
  names(qx) <- sub("CountryCode", "region", names(qx))
  qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  qx_bu <- select(qx_bu, -c("model", "scenario"))
  ## assign to countries with NA, their H12 region mean
  value.x <- NULL
  value.y <- NULL
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>% 
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>% 
    select(-c("value.x", "value.y"))
  ## assign to countries that still have NA, the global mean
  qx_bu <- qx
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "variable"))
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>% 
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>% 
    select(-c("value.x", "value.y"))
  z <- qx
  
  # Multiplying each power plant tech by availability rate (from PRIMES)
  qx <- qx %>%
    mutate(value = case_when(
    variable == "ATHLGN" ~ value * 0.85,
    variable == "ATHHCL" ~ value * 0.85,
    variable == "ATHRFO" ~ value * 0.80,
    variable == "ATHNGS" ~ value * 0.80,
    variable == "ATHBMSWAS" ~ value * 0.85,
    variable == "ACCGT" ~ value * 0.80,
    variable == "PGNUC" ~ value * 0.90,
    variable == "PGLHYD" ~ value * 0.67,
    variable == "PGWND" ~ value * 0.23,
    variable == "PGSOL" ~ value * 0.20,
    variable == "PGASOL" ~ value * 0.25,
    variable == "PGAWNO" ~ value * 0.32,
    variable == "PGAOTHREN" ~ value * 0.45,
    TRUE ~ value ))
  
  qx <- mutate(qx, value = sum(value, na.rm = TRUE), .by = c("region", "period"))
  qx["variable"] <- "TOTCAP"
  qx <- unique(qx)
  x <- as.quitte(qx) %>% as.magpie()
  # set NA to 0
  x[is.na(x)] <- 0
  
  list(x = x,
       weight = NULL,
       unit = "MW",
       description = "Enerdata; Installed capacity")
  
}
