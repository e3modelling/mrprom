#' calcISupRateEneBranCons
#'
#' Use ENERDATA Own Use of Energy Industries divided by Production data to 
#' derive the Rate of Energy Branch Consumption over total transformation output.
#' The output is the OPENPROM input parameter iSupRateEneBranCons.
#'
#' @return  OPENPROM input data ISupRateEneBranCons.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "ISupRateEneBranCons", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte interpolate_missing_periods


calcISupRateEneBranCons <- function() {
  
  # load data source (ENERDATA)
  x <- readSource("ENERDATA", "own", convert = TRUE)
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  x <- x[, c(max(fStartHorizon, min(getYears(x, as.integer = TRUE))) : max(getYears(x, as.integer = TRUE))), ]
  
  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-enerdata-ownuse-mapping.csv",
                        type = "sectoral",
                        where = "mappingfolder")
  
  ## ..and only items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  map <- map[map[, "ENERDATA"] %in% enernames, ]
  
  z <- enernames == "Oil products own use of energy industries.Mtoe - Motor gasoline own use of energy industries.Mtoe - Diesel, heating oil own use of energy industries.Mtoe - Heavy fuel oil own use of energy industries.Mtoe - LPG (liquified petroleum gas) own use of energy industries.Mtoe - Kerosene own use of energy industries.Mtoe"
  enernames[z] <- "Oil products own use of energy industries.Mtoe"
  k <- enernames == "Gas own use of energy industries.Mtoe - Natural gas own use of energy industries.Mtoe"
  enernames[k] <- "Gas own use of energy industries.Mtoe"
  
  x <- x[, , enernames]
  
  b1 <- x[, , "Motor gasoline own use of energy industries.Mtoe"]
  b2 <- x[, , "Diesel, heating oil own use of energy industries.Mtoe"]
  b3 <- x[, , "Heavy fuel oil own use of energy industries.Mtoe"]
  b4 <- x[, , "LPG (liquified petroleum gas) own use of energy industries.Mtoe"]
  b5 <- x[, , "Kerosene own use of energy industries.Mtoe"]
  c <- x[, , "Natural gas own use of energy industries.Mtoe"]
  
  x[, , "Oil products own use of energy industries.Mtoe"] <- x[, , "Oil products own use of energy industries.Mtoe"] - ifelse(is.na(b1), 0, b1) - ifelse(is.na(b2), 0, b2) - ifelse(is.na(b3), 0, b3) - ifelse(is.na(b4), 0, b4) - ifelse(is.na(b5), 0, b5)
  x[, , "Gas own use of energy industries.Mtoe"] <- x[, , "Gas own use of energy industries.Mtoe"] - ifelse(is.na(c), 0, c)
  
  ## rename variables from ENERDATA to openprom names
  ff <- map[!(map[, 2] == ""), 2]
  getNames(x) <- paste0(ff,".Mtoe")
  
  # load data source (ENERDATA)
  y <- readSource("ENERDATA", "production", convert = TRUE)
  y <- y[, c(max(fStartHorizon, min(getYears(y, as.integer = TRUE))) : max(getYears(y, as.integer = TRUE))), ]
  
  d <- y[, , "Nuclear electricity production.GWh"]
  
  e1 <- y[, , "Motor gasoline production.Mtoe"]
  e2 <- y[, , "Diesel, heating oil production.Mtoe"]
  e3 <- y[, , "Heavy fuel oil production.Mt"]
  e4 <- y[, , "LPG (including ethane before 1990) production.Mt"]
  e5 <- y[, , "Kerosene production.Mt"]
  
  x[,,"HCL.Mtoe"] <- x[,,"HCL.Mtoe"] / y[, , "Primary production of Coal and lignite.Mtoe"]
  x[,,"CRO.Mtoe"] <- x[,,"CRO.Mtoe"] / y[, , "Primary production of crude oil, NGL.Mtoe"]
  x[,,"GSL.Mtoe"] <- x[,,"GSL.Mtoe"] / y[, , "Motor gasoline production.Mtoe"]
  x[,,"GDO.Mtoe"] <- x[,,"GSL.Mtoe"] / y[, , "Diesel, heating oil production.Mtoe"]
  x[,,"RFO.Mtoe"] <- x[,,"RFO.Mtoe"] / y[, , "Heavy fuel oil production.Mt"]
  x[,,"LPG.Mtoe"] <- x[,,"LPG.Mtoe"] / y[, , "LPG (including ethane before 1990) production.Mt"]
  x[,,"OLQ.Mtoe"] <- x[,,"OLQ.Mtoe"] / (y[, , "Oil products production.Mt"] - ifelse(is.na(e1), 0, e1) - ifelse(is.na(e2), 0, e2) - ifelse(is.na(e3), 0, e3) - ifelse(is.na(e4), 0, e4) - ifelse(is.na(e5), 0, e5))
  x[,,"NGS.Mtoe"] <- x[,,"NGS.Mtoe"] / y[, , "Primary production of natural gas.Mtoe"]
  x[,,"ELC.Mtoe"] <- x[,,"ELC.Mtoe"] / ((y[,,"Electricity production.GWh"] + ifelse(is.na(d), 0, d)) / 1000 * 0.086)
  x[,,"OGS.Mtoe"] <- 0
  x[,,"LGN.Mtoe"] <- 0
  
  # Adding the PROM variables with placeholder values
  x <- add_columns(x, addnm = "STE", dim = "variable", fill = 1)
  x <- add_columns(x, addnm = "BMSWAS", dim = "variable", fill = 0)
  
  x[is.infinite(x)] <- 0
  x[is.na(x)] <- 0
  
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = 2010 : 2100, expand.values = TRUE)
  variable <- NULL
  
  qx <- qx %>% filter(variable != "KRS")
  
  qx_bu <- qx
  # assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  names(qx) <- sub("region", "CountryCode", names(qx))
  ## add h12 mapping to dataset
  qx <- left_join(qx, h12, by = "CountryCode")
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
  x <- as.quitte(qx) %>% as.magpie()
  # set NA to 0
  x[is.na(x)] <- 0
  
  list(x = x,
       weight = NULL,
       unit = "Rate",
       description = "Enerdata; Rate of Energy Branch Consumption over total transformation output")
  
}