#' calcIFuelCons
#'
#' Use ENERDATA fuel consumption data to derive OPENPROM input parameter iFuelConsXXX
#' (XXX: NENSE, INDSE, DOMSE, TRANSE)
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
#' @importFrom dplyr filter %>% mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte
#' @importFrom utils tail


calcIFuelCons <- function(subtype = "DOMSE") {

  # load data source (ENERDATA)
  x <- readSource("ENERDATA", "consumption", convert = TRUE)

  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  lastYear <- sub("y", "", tail( sort(getYears(x)), 1))
  x <- x[, c(fStartHorizon:lastYear), ]

  # load current OPENPROM set configuration
  sets <- readSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), subtype)
  sets <- unlist(strsplit(sets[, 1], ","))

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
  ## for oil, rename unit from Mt to Mtoe
  if(any(grepl("oil", getItems(x, 3.1)) & grepl("Mt$", getNames(x)))) {
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
  if (subtype == "TRANSE") {

    a <- readSource("IRF", subtype = "total-van,-pickup,-lorry-and-road-tractor-traffic")
    #million motor vehicle km/yr
    a2 <- readSource("IRF", subtype = "passenger-car-traffic")
    #million motor vehicle km/yr
    a3 <- readSource("IRF", subtype = "bus-and-motor-coach-traffic")
    #million motor vehicle km/yr
    a4 <- readSource("ENERDATA", subtype =  "diesel")
    a4 <- a4[, , "Diesel final consumption of transport (excl biodiesel)"][, , "Mtoe"]
    #Mtoe, Millions of tonnes of oil equivalent
    a5 <- readSource("ENERDATA", subtype =  "total")
    a5 <- a5[, , "Total energy final consumption of transport"][, , "Mtoe"]
    #Mtoe, Millions of tonnes of oil equivalent

    a <- a[, Reduce(intersect, list(getYears(a), getYears(a2), getYears(a3), getYears(a4), getYears(a5))), ]#million motor vehicle km/yr
    a2 <- a2[, Reduce(intersect, list(getYears(a), getYears(a2), getYears(a3), getYears(a4), getYears(a5))), ]#million motor vehicle km/yr
    a3 <- a3[, Reduce(intersect, list(getYears(a), getYears(a2), getYears(a3), getYears(a4), getYears(a5))), ]#million motor vehicle km/yr
    a4 <- a4[, Reduce(intersect, list(getYears(a), getYears(a2), getYears(a3), getYears(a4), getYears(a5))), ]#Mtoe
    a5 <- a5[, Reduce(intersect, list(getYears(a), getYears(a2), getYears(a3), getYears(a4), getYears(a5))), ]#Mtoe

    out1 <- ((a4 * a4) / a5)
    out2 <- (a2 / (a + a3))
    x2 <- out1 * out2
    x2 <- collapseNames(x2)
    getNames(x2) <- "PC.GDO.Mtoe"
    getSets(x2) <- c("region", "period", "variable", "new", "unit")
    x <- mbind(x[, intersect(getYears(x), getYears(x2)), ], x2[, intersect(getYears(x), getYears(x2)), ])

    a6 <- readSource("IRF", subtype = "inland-surface-passenger-transport-by-rail")
    #million pKm/yr
    a7 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-rail")
    #million tKm/yr
    a6 <- a6[, Reduce(intersect, list(getYears(a6), getYears(a7), getYears(x))), ]
    a7 <- a7[, Reduce(intersect, list(getYears(a6), getYears(a7), getYears(x))), ]
    x <- x[, Reduce(intersect, list(getYears(a6), getYears(a7), getYears(x))), ]


    x[, , "PT.GDO.Mtoe"] <- x[, , "PT.GDO.Mtoe"] * (a6 / (a6 + a7))
    x[, , "GT.GDO.Mtoe"] <- x[, , "GT.GDO.Mtoe"] * (a7 / (a6 + a7))

    x[, , "PT.ELC.Mtoe"] <- x[, , "PT.ELC.Mtoe"] * (a6 / (a6 + a7))
    x[, , "GT.ELC.Mtoe"] <- x[, , "GT.ELC.Mtoe"] * (a7 / (a6 + a7))


    a8 <- readSource("IRF", subtype = "inland-surface-public-passenger-transport-by-road")
    #million pKm/yr
    a9 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-road")
    #million tKm/yr
    a8 <- a8[, Reduce(intersect, list(getYears(a8), getYears(a9), getYears(x))), ]
    a9 <- a9[, Reduce(intersect, list(getYears(a8), getYears(a9), getYears(x))), ]
    x <- x[, Reduce(intersect, list(getYears(a8), getYears(a9), getYears(x))), ]


    x[, , "PC.GSL.Mtoe"] <- x[, , "PC.GSL.Mtoe"] * (a8 / (a8 + a9))
    x[, , "GU.GSL.Mtoe"] <- x[, , "GU.GSL.Mtoe"] * (a9 / (a8 + a9))

    x[, , "PC.NGS.Mtoe"] <- x[, , "PC.NGS.Mtoe"] * (a8 / (a8 + a9))
    x[, , "GU.NGS.Mtoe"] <- x[, , "GU.NGS.Mtoe"] * (a9 / (a8 + a9))

    x[, , "PC.ELC.Mtoe"] <- x[, , "PC.ELC.Mtoe"] * (a8 / (a8 + a9))
    x[, , "GU.ELC.Mtoe"] <- x[, , "GU.ELC.Mtoe"] * (a9 / (a8 + a9))
  }

   # complete incomplete time series
  qx <- as.quitte(x) %>%
       interpolate_missing_periods(period = getYears(x, as.integer = TRUE), expand.values = TRUE)
  # assign to countries with NA, their H12 region mean with weights
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
 
  qx <- select(qx, -c("model", "scenario"))
  qx_bu <- qx
  
  ## assign to countries with NA, their H12 region mean with weights
  
  population <- calcOutput(type = "POP", aggregate = FALSE)
  population <- as.quitte(population)
  
  # compute weights by population
  names(population) <- sub("region", "CountryCode", names(population))
  ## add mapping to dataset
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
  ## assign to countries that still have NA, the global mean with weights
  qx_bu <- qx
  
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

  list(x = x,
       weight = NULL,
       unit = "various",
       description = "Enerdata; fuel consumption in XXX sector")

}
