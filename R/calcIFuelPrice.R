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
  
  x <- readSource("IEATOTPRICES", convert = TRUE)
  x[x == 0] <- NA # set all zeros to NA because we deal with prices
  x <- x[, c(fStartHorizon : max(getYears(x, as.integer = TRUE))), ]
  x <- collapseDim(x, 3.4)

  map <- filter(map0,!is.na(map0[,"IEA"]))
  map <- filter(map,!is.na(map[,"FUEL"]))

  names(map) <- sub("FUEL","fuel",names(map))
  names(map) <- sub("IEA","variable",names(map))
  qx <- left_join(as.quitte(x), map, by = c("fuel","variable"), relationship = "many-to-many") 
  qx <- filter(qx,!is.na(qx[,"SBS"]))
  qx <- filter(qx,!is.na(qx[,"EF"]))
  qx <- select(qx,"region", "unit", "period", "value", "SBS", "EF")

  x <- as.quitte(qx) %>% as.magpie()
  
  #fix units by Dionisis
  x[,,"Currency/MWh"] <- x[,,"Currency/MWh"] * 11.63
  x[,,"Currency/l"] <- x[,,"Currency/l"] * 1173
  x[,,"Currency/1000 l"] <- x[,,"Currency/1000 l"] * 1.173
  x[,,"Currency/t"][,,c("HCL","LGN","STE1AL","STE1AH","STE2LGN")] <- x[,,"Currency/t"][,,c("HCL","LGN","STE1AL","STE1AH","STE2LGN")] * 42
  x[,,"Currency/t"][,,c("OLQ","STE2OLQ")] <- x[,,"Currency/t"][,,c("OLQ","STE2OLQ")] / 1.38
  x[,,"Currency/MWh (GCV)"] <- x[,,"Currency/MWh (GCV)"] * 11.63
  x[,,"Currency/t"][,,c("BMSWAS","STE2BMS")] <- x[,,"Currency/t"][,,c("BMSWAS","STE2BMS")] / 0.4
  
  x <- collapseDim(x,3.1)
  
  x <- as.quitte(x)
  
  x <- select(x,-variable)
  names(x) <- sub("sbs", "variable", names(x))
  names(x) <- sub("ef", "new", names(x))

  x <- as.quitte(x) %>% as.magpie()
  
  # complete incomplete time series
  x <- as.quitte(x) %>%
    interpolate_missing_periods(period = getYears(x, as.integer = TRUE), expand.values = TRUE) %>%
    as.magpie()

  # # assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  qx <- as.quitte(x)
  qx_bu <- as.quitte(x)
  names(qx) <- sub("region", "CountryCode", names(qx))
  # ## add h12 mapping to dataset
  qx <- left_join(qx, h12, by = "CountryCode")
  # ## add new column containing regional mean value
  value <- NULL
  # qx <- select(qx,-variable)
  # names(qx) <- sub("sbs", "variable", names(qx))
  # names(qx) <- sub("ef", "new", names(qx))
  # qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "new", "variable"))
  # names(qx) <- sub("CountryCode", "region", names(qx))
  # qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  # qx_bu <- select(qx_bu, -c("model", "scenario"))
  # ## assign to countries with NA, their H12 region mean
  # value.x <- NULL
  # value.y <- NULL
  # qx_bu <- select(qx_bu,-variable)
  # names(qx_bu) <- sub("sbs", "variable", names(qx_bu))
  # names(qx_bu) <- sub("ef", "new", names(qx_bu))
  # qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "new", "unit")) %>%
  #   mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
  #   select(-c("value.x", "value.y"))
  # ## assign to countries that still have NA, the global mean
  # qx_bu <- qx
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "new", "variable"))
  qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  qx_bu <- select(qx_bu, -c("model", "scenario"))
  names(qx) <- sub("CountryCode", "region", names(qx))
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

  #add for H2F the biggest value of fuel that has each subsector
  H2F <- add_columns(x, addnm = "H2F", dim = 3.2, fill = 10^-6)
  
  H2F <- as.quitte(H2F)
  
  H2F <- mutate(H2F, value = max(value, na.rm = TRUE), .by = c("region", "period", "variable", "unit"))
  
  H2F <- H2F[which(H2F[,"new"] == "H2F"),]
  
  H2F <- as.quitte(H2F) %>% as.magpie()
  
  x <- mbind(x, H2F)
  
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
