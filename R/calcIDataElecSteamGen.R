#' calcIDataElecSteamGen
#'
#' Use data to derive OPENPROM input parameter iDataElecSteamGen
#'
#' @return  OPENPROM input data iDataElecSteamGen
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
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
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  map <- map[map[, "ENERDATA"] %in% enernames, ]

  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  enernames <- enernames[! enernames %in% c("", 'Single fired electricity capacity coal')]
  enernames[3] <- "Total electricity capacity gas (multifuel oil/gas included)"
  x <- x[, , enernames]
  x[, ,enernames[3]] <- x[, , enernames[3]] - x[, ,enernames[5]]
  
  #getNames(x)[3] <- "Total electricity capacity gas (multifuel oil/gas included).MW - Installed capacity in combined cycles.MW"
  ## rename variables from ENERDATA to openprom names
  ff <- map[!(map[,2]==""), 1]
  ff <- ff[! ff %in% c("ATHHCL")]
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
  qx <- mutate(qx, value = sum(value, na.rm = TRUE), .by = c("region"))
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