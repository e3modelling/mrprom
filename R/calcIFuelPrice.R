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

  # load data source (ENERDATA)
  x <- readSource("ENERDATA", "constant price", convert = TRUE)
  x[x == 0] <- NA # set all zeros to NA because we deal with prices

  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fStartY <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartY"]
  x <- x[, c(fStartHorizon : max(getYears(x, as.integer = TRUE))), ]

  # filter data to choose correct (sub)sectors and fuels
  out <- NULL
  for (i in c("NENSE", "DOMSE", "INDSE", "TRANSE")) { # define main OPEN-PROM sectors that we need data for
    # load current OPENPROM set configuration for each sector
    sets <- readSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), i)
    sets <- unlist(strsplit(sets[,1],","))

    # use enerdata-openprom mapping to extract correct data from source
    map <- toolGetMapping(name = "prom-enerdata-fuprice-mapping.csv",
                          type = "sectoral",
                          where = "mappingfolder")

    ## filter mapping to keep only i sectors
    map <- filter(map, map[, "SBS"] %in% sets)
    ## ..and only items that have an enerdata-prom mapping
    enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
    map <- map[map[, "ENERDATA"] %in% enernames, ]
    ## rename variables from ENERDATA to openprom names (also add new openprom names not existing in ENERDATA)
    ff <- paste(map[, 2], map[, 3], sep = ".")
    iii <- 0
    ### add a dummy dimension to data because mapping has 3 dimensions, and data only 2
    for (ii in map[, "ENERDATA"]) {
      iii <- iii + 1
      out <- mbind(out, setNames(add_dimension(x[, , ii], dim = 3.2), paste0(ff[iii], ".", sub("^.*.\\.", "", getNames(x[,, ii])))))
    }
  }
  ### add new openprom names not existing in ENERDATA
  out <- complete_magpie(out)
  out[, , "HOU"] <- 2 * out[, , "IS"]
  # AG/SE = HOU
  # NEN = PCH
  tmp <- out[, , "HOU"]
  getNames(tmp) <- sub("HOU", "AG", getNames(tmp))
  out <- mbind(out, tmp)
  getNames(tmp) <- sub("AG", "SE", getNames(tmp))
  out <- mbind(out, tmp)
  tmp <- out[, , "PCH"]
  getNames(tmp) <- sub("PCH", "NEN", getNames(tmp))
  out <- mbind(out, tmp)
  out[, , "OLQ"] <- out[, , "RFO"]

  # complete incomplete time series
  x <- as.quitte(out) %>%
       interpolate_missing_periods(period = getYears(out, as.integer = TRUE), expand.values = TRUE) %>%
       as.magpie() %>%
       complete_magpie()
  
  # assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv")
  qx <- as.quitte(x)
  qx_bu <- as.quitte(x)
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
