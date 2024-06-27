#' calcIFuelCons
#'
#' Use ENERDATA and IEA fuel consumption data to derive OPENPROM input parameter iFuelConsXXX
#' (XXX: NENSE, INDSE, DOMSE, TRANSE). If both sources has data about the same fuel 
#' the data from ENERDATA is taken.
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
#' @importFrom R.utils isZero


calcIFuelCons <- function(subtype = "DOMSE") {

  # load data source (ENERDATA)
  x <- readSource("ENERDATA", "consumption", convert = TRUE)

  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  lastYear <- sub("y", "", tail(sort(getYears(x)), 1))
  x <- x[, c(fStartHorizon:lastYear), ]

  # load current OPENPROM set configuration
  sets <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), subtype)
  sets <- unlist(strsplit(sets[, 1], ","))

  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-enerdata-fucon-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")
  maps <- map
  ## filter mapping to keep only XXX sectors
  map <- filter(map, map[, "SBS"] %in% sets)
  ## ..and only items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  map <- map[map[, "ENERDATA"] %in% enernames, ]
  ## filter data to keep only XXX data
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  x <- x[, , enernames]
  ## for oil, rename unit from Mt to Mtoe
  if (any(grepl("oil", getItems(x, 3.1)) & grepl("Mt$", getNames(x)))) {
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
  
  IEA <- NULL
  q <- as.quitte(x)
  map <- map %>% drop_na(IEA)
  
  #the map has a column SBS which corresponds to flow of IEA
  for (ii in unique(map[, "flow"])) {
    d <- readSource("IEA", subtype = as.character(ii))
    d <- d / 1000 #ktoe to mtoe
    d <- as.quitte(d)
    #each flow has some products as it is the EF column of map
    m <- filter(map, map[["flow"]] == ii)
    #for each product of IEA data
    region <- NULL
    period <- NULL
    for (i in 1 : nrow(m)) {
      #filter the IEA data (of a specific flow) with product
      qb <- filter(d, d[["product"]] == m[i, 4])
      qb <- select((qb), c(region, period, value))
      #flow MARBUNK has negative values
      if (ii == "MARBUNK") {
        qb["value"] <- - qb["value"]
      }
      #join ENERDATA and IEA
      q <- left_join(q, qb, by = c("region", "period"))
      #if ENERDATA is na take the value of IEA
      q[which(q[, 8] == m[i, 3] & q[, 4] == m[i, 2]), ] <- q[which(q[, 8] == m[i, 3] & q[, 4] == m[i, 2]), ] %>% mutate(`value.x` = ifelse(is.na(`value.x`), `value.y`, `value.x`))
      q[which(q[, 8] == m[i, 3] & q[, 4] == m[i, 2]), ] <- q[which(q[, 8] == m[i, 3] & q[, 4] == m[i, 2]), ] %>% mutate(`value.x` = ifelse((isZero(`value.x`) & !isZero(`value.y`)& !is.na(`value.y`)), `value.y`, `value.x`))
      names(q) <- sub("value.x", "value", names(q))
      q <- select((q), -c(`value.y`))
    }
    #add data of IEA which ENERDATA does not have
    #remove rows from map that compared with ENERDATA in the previous step
    l <- !(do.call(paste0, maps) %in% do.call(paste0, map))
    map2 <- maps[l, ]
    map2 <- map2 %>% drop_na(IEA)
    #for a specific flow take the products
    mapping <- map2[which(map2["flow"] == ii), 4]
    ##filter the IEA data (of a specific flow) with products
    qy <- filter(d, d[["product"]] %in% mapping)
    if (ii == "MARBUNK") {
      qy["value"] <- - qy["value"]
    }
    names(map2) <- sub("IEA", "product", names(map2))
    #rename from IEA names to OPEN PROM names
    qy <- left_join(qy, map2, by = c("product", "flow"))
    qy["variable"] <- qy["SBS"]
    qy["new"] <- qy["EF"]
    qy["unit"] <- "Mtoe"
    qy <- select(qy, -c("SBS", "EF", "ENERDATA", "product", "flow"))
    #ENERDATA has values 2010:2021 
    qy <- filter(qy, qy[["period"]] %in% fStartHorizon : tail(getYears(x, as.integer = TRUE), n = 1))
    q <- rbind(q, qy)
  }
  
  if (subtype == "TRANSE") {
    #variable, new , unit
    q <- q[, c(1, 2, 3, 4, 8 , 5 , 6 , 7)]
  }
  
  x <- as.magpie(q)
  
  if (subtype == "TRANSE") {

    a6 <- readSource("IRF", subtype = "inland-surface-passenger-transport-by-rail")
    #million pKm/yr
    a7 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-rail")
    #million tKm/yr
    a6 <- a6[, Reduce(intersect, list(getYears(a6), getYears(a7), getYears(x))), ]
    a7 <- a7[, Reduce(intersect, list(getYears(a6), getYears(a7), getYears(x))), ]
    x <- x[, Reduce(intersect, list(getYears(a6), getYears(a7), getYears(x))), ]

    #inland-surface-passenger-transport-by-rail / total inland-surface transport-by-rail
    x[, , "PT.GDO.Mtoe"] <- x[, , "PT.GDO.Mtoe"] * (a6 / (a6 + a7))
    #inland-surface-freight-transport-by-rail / total inland-surface
    x[, , "GT.GDO.Mtoe"] <- x[, , "GT.GDO.Mtoe"] * (a7 / (a6 + a7))

    x[, , "PT.ELC.Mtoe"] <- x[, , "PT.ELC.Mtoe"] * (a6 / (a6 + a7))
    x[, , "GT.ELC.Mtoe"] <- x[, , "GT.ELC.Mtoe"] * (a7 / (a6 + a7))


    a8 <- readSource("IRF", subtype = "passenger-car-traffic")
    #million pKm/yr
    a9 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-road")
    #million tKm/yr
    a8 <- a8[, Reduce(intersect, list(getYears(a8), getYears(a9), getYears(x))), ]
    a9 <- a9[, Reduce(intersect, list(getYears(a8), getYears(a9), getYears(x))), ]
    x <- x[, Reduce(intersect, list(getYears(a8), getYears(a9), getYears(x))), ]

    #inland-surface-freight-transport-by-road / total inland-surface-transport-by-road

    x[, , "PC.GDO.Mtoe"] <- x[, , "PC.GDO.Mtoe"] * (a8 / (a8 + a9))
    x[, , "GU.GDO.Mtoe"] <- x[, , "GU.GDO.Mtoe"] * (a9 / (a8 + a9))
    
    l <- getNames(x) == "PA.KRS.Mt"
    getNames(x)[l] <- "PA.KRS.Mtoe"
    #from Mt to Mtoe
    x[,,"PA.KRS.Mtoe"] <- x[,,"PA.KRS.Mtoe"] / 1.027
  }

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

  list(x = x,
       weight = NULL,
       unit = "various",
       description = "Enerdata; fuel consumption in XXX sector")

}
