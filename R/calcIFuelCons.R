#' calcIFuelCons
#'
#' Use ENERDATA fuel consumption data to derive OPENPROM input parameter iFuelConsXXX
#' (XXX: NENSE, INDSE, DOMSE)
#'
#' @param subtype string, OPENPROM sector (DOMSE, INDSE, NENSE)
#' @return  OPENPROM input data iFuelConsXXX
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IFuelCons", subtype = "DOMSE", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte


calcIFuelCons <- function(subtype = "DOMSE") {

  # load data source (ENERDATA)
  x <- readSource("ENERDATA", "consumption", convert = TRUE)

  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fStartY <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartY"]
  x <- x[, c(fStartHorizon:fStartY), ]

  # load current OPENPROM set configuration
  sets <- readSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), subtype)
  sets <- unlist(strsplit(sets[,1],","))

  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-enerdata-fucon-mapping.csv",
                        type = "sectoral",
                        where = "mappingfolder")

  ## filter mapping to keep only XXX sectors
  map <- filter(map, map[, "SBS"] %in% sets)
  ## ..and only items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  map <- map[map[, "ENERDATA"] %in% enernames, ]
  ## filter data to keep only XXX data
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  x <- x[, , enernames]
  ## rename variables to openprom names
  ### add a dummy dimension to data because mapping has 3 dimensions, and data ony 2
  x <- add_dimension(x, dim = 3.2)
  ### rename variables
  getNames(x) <- paste0(paste(map[, 2], map[, 3], sep = "."), ".", sub("^.*.\\..*.\\.", "", getNames(x)))


  # complete incomplete time series
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
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "new", "variable"))
  names(qx) <- sub("CountryCode", "region", names(qx))
  qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  qx_bu <- select(qx_bu, -c("model", "scenario"))
  ## assign to countries with NA, their H12 region mean
  value.x <- NULL
  value.y <- NULL
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "new", "unit")) %>% 
         mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>% 
         select(-c("value.x", "value.y"))
  ## assign to countries that still have NA, the global mean
  qx_bu <- qx
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "new", "variable"))
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "new", "unit")) %>% 
         mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>% 
         select(-c("value.x", "value.y"))
  x <- as.quitte(qx) %>% as.magpie()


  # set NA to 0
  x[is.na(x)] <- 0
  
  if (subtype == "transe"){
    
    a <- readSource("IRF", subtype = "total-van,-pickup,-lorry-and-road-tractor-traffic")
    #million motor vehicle km/yr
    a2 <- readSource("IRF", subtype = "passenger-car-traffic")
    #motor vehicle km/yr
    a3 <- readSource("IRF", subtype = "bus-and-motor-coach-traffic")
    #km/yr
    a4 <- readSource("ENERDATA", subtype =  "diesel")
    q <- as.quitte(a4)
    variable <- NULL
    unit <- NULL
    q <- filter(q, variable == "Diesel final consumption of transport (excl biodiesel)")
    q <- filter(q, unit == "Mtoe")
    q2 <- as.magpie(q)
    #Mtoe, Millions of tonnes of oil equivalent 
    a5 <- readSource("ENERDATA", subtype =  "total")
    q3 <- as.quitte(a5)
    q3 <- filter(q3, variable == "Total energy final consumption of transport")
    q3 <- filter(q3, unit == "Mtoe")
    q4 <- as.magpie(q3)
    #Mtoe, Millions of tonnes of oil equivalent 
    
    a <- a[,Reduce(intersect, list(getYears(a),getYears(a2),getYears(a3),getYears(q2),getYears(q4))),]#million motor vehicle km/yr
    a2 <- a2[,Reduce(intersect, list(getYears(a),getYears(a2),getYears(a3),getYears(q2),getYears(q4))),]#motor vehicle km/yr
    a3 <- a3[,Reduce(intersect, list(getYears(a),getYears(a2),getYears(a3),getYears(q2),getYears(q4))),]#km/yr
    q2 <- q2[,Reduce(intersect, list(getYears(a),getYears(a2),getYears(a3),getYears(q2),getYears(q4))),]#Mtoe
    q4 <- q4[,Reduce(intersect, list(getYears(a),getYears(a2),getYears(a3),getYears(q2),getYears(q4))),]#Mtoe
    
    out1 <- ((q2*q2)/q4)
    out2 <- (a2/(a/10^6+a3))
    x <- out1*out2
    qu <- as.quitte(x)
    
  }

  list(x = x,
       weight = NULL,
       unit = "various",
       description = "Enerdata; fuel consumption in XXX sector")

}
