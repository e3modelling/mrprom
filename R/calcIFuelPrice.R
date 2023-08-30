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
#' @importFrom dplyr filter %>%
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte


calcIFuelPrice <- function() {

  # load data source (ENERDATA)
  x <- readSource("ENERDATA", "constant price", convert = TRUE)
  x[x == 0] <- NA # set all zeros to NA because we deal with prices

  # filter years
  years <- toolReadSetFromGDX(system.file(file.path("extdata", "blabla.gdx"), package = "mrprom"), "datay")
  x <- x[, c(min(as.numeric(years[, "a"])) : max(getYears(x, as.integer = TRUE))), ]

  xtmp <- NULL
  out <- NULL
  for (i in c("NENSE", "DOMSE", "INDSE")) { # define main OPEN-PROM sectors that we need data for
    # load current OPENPROM set configuration for each sector
    sets <- toolReadSetFromGDX(system.file(file.path("extdata", "fulldata.gdx"), package = "mrprom"), i)

    # use enerdata-openprom mapping to extract correct data from source
    map <- toolGetMapping(name = "prom-enerdata-fuprice-mapping.csv",
                          type = "sectoral",
                          where = "mappingfolder")

    ## filter mapping to keep only i sectors
    map <- filter(map, map[, "SBS"] %in% sets[, 1])
    ## ..and only items that have an enerdata-prom mapping
    enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
    map <- map[map[, "ENERDATA"] %in% enernames, ]
    ## filter data to keep only i data
    enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
    tmp <- x[, , enernames]
    ## rename variables from ENERDATA to openprom names (also add new openprom names not existing in ENERDATA)
    ### add a dummy dimension to data because mapping has 3 dimensions, and data ony 2
    tmp <- add_dimension(tmp, dim = 3.2)
    ### rename variables
    if (length(getNames(tmp)) < length(paste(map[, 2], map[, 3], sep = "."))) {
      xtmp2 <- tmp
      for (ii in c(1:(length(paste(map[, 2], map[, 3], sep = ".")) / length(getNames(tmp))))) {
        getNames(xtmp2) <- paste0(paste(map[, 2], map[, 3], sep = ".")[seq(length(getNames(xtmp2))*(ii-1)+1,length(getNames(xtmp2))*ii,1)], ".", sub("^.*.\\..*.\\.", "", getNames(xtmp2))) # nolint
        if (ii == 1) {
          tmp <- mbind(out, xtmp2)
        } else {
          tmp <- mbind(tmp, xtmp2)
        }
      }

    } else {
      getNames(tmp) <- paste0(paste(map[, 2], map[, 3], sep = "."), ".", sub("^.*.\\..*.\\.", "", getNames(tmp)))
    }
  out <- mbind(xtmp, tmp, out)
  }
  out[, , "HOU"] <- 2 * out[, , "IS"]
  x <- as.quitte(out[,,"HOU"]) %>%
       interpolate_missing_periods(period = getYears(out, as.integer = TRUE), expand.values = TRUE) %>%
       as.magpie()

  h12 <- toolGetMapping("regionmappingH12.csv")
  x_bu <- x[c("CHN", "IND", "USA", "JPN"), , ]
  x[is.na(x)] <- 0
  weight <- x
  
  tmp <- toolAggregate(x, weight = weight, rel = h12, from = "CountryCode", to = "RegionCode") #nolint
  tmp[tmp==0] <- NA

#  for (i in unique(h12$RegionCode)[!unique(h12$RegionCode)%in%getRegions(x_bu)][-9]) {

#  }


#  as.quitte(tmp) %>%
#  select(c("region", "variable", "unit", "period", "value", "new")) %>%
#  mutate(avg = mean(value, na.rm = TRUE), .by = "region")


  x <- toolAggregate(tmp, weight= NULL, partrel = TRUE , mixed_aggregation = TRUE , rel = h12, to = "CountryCode", from = "RegionCode") #nolint
  x <- mbind(x_bu, x[c(getRegions(x_bu), "CHA"), , , invert = TRUE])

  # for those countries where sectoral activity is 0, set price to NA
  # for those countries where price is NA (and actv != 0) find value from same H12 region

  list(x = x,
       weight = NULL,
       unit = "various",
       description = "Enerdata; fuel price in all sectors")

}
