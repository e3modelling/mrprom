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
  fStartHorizon <- toolReadEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]

  # use enerdata-openprom mapping to extract correct data from source
  map0 <- toolGetMapping(
    name = "prom-IEA-fuprice-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  )

  x <- readSource("IEATOTPRICES", convert = TRUE)
  years <- getYears(x, as.integer = TRUE)
  x[x == 0] <- NA # set all zeros to NA because we deal with prices
  x <- x[, c(fStartHorizon:max(years)), ]

  map <- filter(map0, !is.na(map0[, "IEA"]))
  map <- filter(map, !is.na(map[, "FUEL"]))

  names(map) <- sub("FUEL", "fuel", names(map))
  names(map) <- sub("IEA", "variable", names(map))
  qx <- left_join(as.quitte(x), map, by = c("fuel", "variable"), relationship = "many-to-many") %>%
    filter(!is.na(SBS), !is.na(EF)) %>%
    select("region", "unit", "period", "value", "SBS", "EF")

  x <- as.quitte(qx)
  x <- select(x, -variable)
  names(x) <- sub("sbs", "variable", names(x))
  names(x) <- sub("ef", "new", names(x))

  # complete incomplete time series
  x <- as.quitte(x) %>%
    interpolate_missing_periods(period = years, expand.values = TRUE) %>%
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
 
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "new", "variable"))
  qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  qx_bu <- select(qx_bu, -c("model", "scenario"))
  names(qx) <- sub("CountryCode", "region", names(qx))
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "new", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  x <- as.quitte(qx) %>% as.magpie()
  # add for H2F the biggest value of fuel that has each subsector
  H2F <- add_columns(x, addnm = "H2F", dim = 3.3, fill = 0)

  H2F <- as.quitte(H2F)

  H2F <- mutate(H2F, value = max(value, na.rm = TRUE), .by = c("region", "period", "variable", "unit"))

  H2F <- H2F[which(H2F[, "new"] == "H2F"), ]

  H2F <- as.quitte(H2F) %>% as.magpie()

  x <- mbind(x, H2F)

  # remove PHEV, CHEV
  items <- getItems(x, 3.2)
  transport_items <- grep("^PHEV|^CHEV", items, value = TRUE)
  x <- x[, , setdiff(getItems(x, 3.2), transport_items)]
  
  SharesFuelPrices <- calcOutput("SharesFuelPrices", aggregate = FALSE)
  SharesFuelPrices <- SharesFuelPrices[,2025,]
  
  MultByShare <- x
  
  MultByShare <- add_columns(MultByShare, addnm = "BGDO", dim = 3.3, fill = NA)
  MultByShare <- add_columns(MultByShare, addnm = "BGSL", dim = 3.3, fill = NA)
  MultByShare <- add_columns(MultByShare, addnm = "BKRS", dim = 3.3, fill = NA)
  
  # BGDO
  MultByShare[,,"PC.USD2015/toe.BGDO"] <- MultByShare[,,"PC.USD2015/toe.GDO"] * (SharesFuelPrices[,,"PC.shareBGDO"])
  # MultByShare[,,"PC.USD2015/toe.GDO"] <- MultByShare[,,"PC.USD2015/toe.GDO"] * (1/sqrt(SharesFuelPrices[,,"PC.shareBGDO"]))
  
  MultByShare[,,"PB.USD2015/toe.BGDO"] <- MultByShare[,,"PB.USD2015/toe.GDO"] * (SharesFuelPrices[,,"PB.shareBGDO"])
  # MultByShare[,,"PB.USD2015/toe.GDO"] <- MultByShare[,,"PB.USD2015/toe.GDO"] * (1/sqrt(SharesFuelPrices[,,"PB.shareBGDO"]))
  
  MultByShare[,,"PT.USD2015/toe.BGDO"] <- MultByShare[,,"PT.USD2015/toe.GDO"] * (SharesFuelPrices[,,"PT.shareBGDO"])
  # MultByShare[,,"PT.USD2015/toe.GDO"] <- MultByShare[,,"PT.USD2015/toe.GDO"] * (1/sqrt(SharesFuelPrices[,,"PT.shareBGDO"]))
  
  MultByShare[,,"PN.USD2015/toe.BGDO"] <- MultByShare[,,"PN.USD2015/toe.GDO"] * (SharesFuelPrices[,,"PN.shareBGDO"])
  # MultByShare[,,"PN.USD2015/toe.GDO"] <- MultByShare[,,"PN.USD2015/toe.GDO"] * (1/sqrt(SharesFuelPrices[,,"PN.shareBGDO"]))
  
  MultByShare[,,"GT.USD2015/toe.BGDO"] <- MultByShare[,,"GT.USD2015/toe.GDO"] * (SharesFuelPrices[,,"GT.shareBGDO"])
  # MultByShare[,,"GT.USD2015/toe.GDO"] <- MultByShare[,,"GT.USD2015/toe.GDO"] * (1/sqrt(SharesFuelPrices[,,"GT.shareBGDO"]))
  
  MultByShare[,,"GN.USD2015/toe.BGDO"] <- MultByShare[,,"GN.USD2015/toe.GDO"] * (SharesFuelPrices[,,"GN.shareBGDO"])
  # MultByShare[,,"GN.USD2015/toe.GDO"] <- MultByShare[,,"GN.USD2015/toe.GDO"] * (1/sqrt(SharesFuelPrices[,,"GN.shareBGDO"]))
  
  MultByShare[,,"GU.USD2015/toe.BGDO"] <- MultByShare[,,"GU.USD2015/toe.GDO"] * (SharesFuelPrices[,,"GU.shareBGDO"])
  # MultByShare[,,"GU.USD2015/toe.GDO"] <- MultByShare[,,"GU.USD2015/toe.GDO"] * (1/sqrt(SharesFuelPrices[,,"GU.shareBGDO"]))
  
  # BGSL
  MultByShare[,,"PC.USD2015/toe.BGSL"] <- MultByShare[,,"PC.USD2015/toe.GSL"] * (SharesFuelPrices[,,"PC.shareBGSL"])
  # MultByShare[,,"PC.USD2015/toe.GSL"] <- MultByShare[,,"PC.USD2015/toe.GSL"] * (1/sqrt(SharesFuelPrices[,,"PC.shareBGSL"]))
  
  MultByShare[,,"PB.USD2015/toe.BGSL"] <- MultByShare[,,"PB.USD2015/toe.GSL"] * (SharesFuelPrices[,,"PB.shareBGSL"])
  # MultByShare[,,"PB.USD2015/toe.GSL"] <- MultByShare[,,"PB.USD2015/toe.GSL"] * (1/sqrt(SharesFuelPrices[,,"PB.shareBGSL"]))
  
  MultByShare[,,"GU.USD2015/toe.BGSL"] <- MultByShare[,,"GU.USD2015/toe.GSL"] * (SharesFuelPrices[,,"GU.shareBGSL"])
  # MultByShare[,,"GU.USD2015/toe.GSL"] <- MultByShare[,,"GU.USD2015/toe.GSL"] * (1/sqrt(SharesFuelPrices[,,"GU.shareBGSL"]))
  
  # BKRS
  MultByShare[,,"PA.USD2015/toe.BKRS"] <- MultByShare[,,"PA.USD2015/toe.KRS"] * (SharesFuelPrices[,,"PA.shareBKRS"])
  # MultByShare[,,"PA.USD2015/toe.KRS"] <- MultByShare[,,"PA.USD2015/toe.KRS"] * (1/sqrt(SharesFuelPrices[,,"PA.shareBKRS"]))
  
  x <- MultByShare
  
  list(
    x = x,
    weight = NULL,
    unit = "various",
    description = "IEA; fuel price in all sectors"
  )
}
