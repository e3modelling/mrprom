#' calcIFuelPrice
#'
#' Use ENERDATA fuel price data to derive OPENPROM input parameter iFuelPrice
#'
#' @return  OPENPROM input data iFuelPrice
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IFuelPrice", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% left_join mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte


calcIFuelPrice <- function() {

  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]

  # use enerdata-openprom mapping to extract correct data from source
  map0 <- toolGetMapping(name = "prom-IEA-fuprice-mapping.csv",
                         type = "sectoral",
                         where = "mrprom")

  a <- NULL
  for (i in c("NENSE", "DOMSE", "INDSE", "TRANSE", "PG")) { # define main OPEN-PROM sectors that we need data for
    sets <- NULL
    # load current OPENPROM set configuration for each sector
    try(sets <- toolGetMapping(paste0(i, ".csv"),
                               type = "blabla_export",
                               where = "mrprom"))
    try(sets <- as.character(sets[, 1]))
    if (length(sets) == 0) sets <- i
    
    x <- readSource("IEAEnergyPrices", subtype = "all")
    x[x == 0] <- NA # set all zeros to NA because we deal with prices
    x <- x[, c(fStartHorizon : max(getYears(x, as.integer = TRUE))), ]
    x <- x[,,"2015USDR"]

    ## filter mapping to keep only i sectors
    map <- filter(map0, map0[, "SBS"] %in% sets)
    map <- filter(map,!is.na(map[,"FUEL"]))

    x <- x[,,unique(map[, "IEA"])]
    names(map) <- sub("FUEL","fuel",names(map))
    qx <- full_join(as.quitte(x), map, by = c("fuel")) 
    qx <- filter(qx,!is.na(qx[,"SBS"]))
    qx <- filter(qx,!is.na(qx[,"EF"]))
    qx <- qx[,c("region","unit","period","value","SBS","EF")]
    qx <- filter(qx,!is.na(qx[,"region"]))
    x <- as.quitte(qx) %>% as.magpie()
    
    #fix units $15/mwh to 
    if (i %in% c("NENSE", "DOMSE", "INDSE", "PG")) {
      x[,,setdiff(getItems(x,3.2),"LPG")] < x[,,setdiff(getItems(x,3.2),"LPG")] * 11.63
      x[,,"LPG"] < x[,,"LPG"] * 1163
    }
    
    if (i %in% c("TRANSE")) {
      x < x * 1163
    }
    
    a <- mbind(a, x)
    }

  # complete incomplete time series
  x <- as.quitte(a) %>%
    interpolate_missing_periods(period = getYears(a, as.integer = TRUE), expand.values = TRUE) %>%
    as.magpie()
  
  

  # assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  qx <- as.quitte(x)
  qx_bu <- as.quitte(x)
  names(qx) <- sub("region", "CountryCode", names(qx))
  ## add h12 mapping to dataset
  qx <- left_join(qx, h12, by = "CountryCode")
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
  tmp <- x[, , "LGN"]
  getNames(tmp) <- gsub("LGN$", "STE1AB", getNames(tmp))
  x <- mbind(x, tmp)
  x[, , "STE1AB"] <- 300
  getNames(tmp) <- gsub("STE1AB", "BMSWAS", getNames(tmp))
  x <- mbind(x, tmp)
  x[, , "BMSWAS"] <- 300
  getNames(tmp) <- gsub("BMSWAS$", "STE2BMS", getNames(tmp))
  x <- mbind(x, tmp)
  x[, , "STE2BMS"] <- 300


  #mutate(qx, h13 = lst[region])
  #mutate(qx1,avg=mean(value,na.rm=T),.by=c("RegionCode","period","new","variable"))

  #for (i in getRegions(x)) {
  #  for (j in getItems(x, 3.1)) {
  #    for (l in getItems(x, 3.3)) {
  #  ob <- filter(h12,RegionCode == filter(h12,CountryCode == i)$RegionCode)$CountryCode
  #  ob <- which(mselect(x[ob,2010,j][,,l]>0))
  #  weight[i,,j][,,l] <- 1/length(ob)
  #    }
  #  }

  #}

  # Aggregate to H12 regions
  #tmp <- toolAggregate(x, weight = weight, rel = h12, from = "CountryCode", to = "RegionCode") #nolint
  #tmp[tmp==0] <- NA



#  for (i in unique(h12$RegionCode)[!unique(h12$RegionCode)%in%getRegions(x_bu)][-9]) {

#  }


#  as.quitte(tmp) %>%
#  select(c("region", "variable", "unit", "period", "value", "new")) %>%
#  mutate(avg = mean(value, na.rm = TRUE), .by = "region")

  # disaggregate back to single countries
  #x <- toolAggregate(tmp, weight= NULL, partrel = TRUE , mixed_aggregation = TRUE , rel = h12, to = "CountryCode", from = "RegionCode") #nolint
  #x <- mbind(x_bu, x[c(getRegions(x_bu), "CHA"), , , invert = TRUE])

  # for those countries where sectoral activity is 0, set price to NA
  # for those countries where price is NA (and actv != 0) find value from same H12 region

  list(x = x,
       weight = NULL,
       unit = "various",
       description = "Enerdata; fuel price in all sectors")

}
