#' calcACTV
#'
#' Derive economic activity data for OPENPROM sectors based on two data sources:
#' transport, traffic, air transport passengers per country and per year (IRF)
#' and Production Level and Unit Cost (GEME3).
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "ACTV", file = "iACTV.csv", aggregate = TRUE)
#' }
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr filter select last

calcACTV <- function() {

  x <- readSource("GEME3", convert = TRUE) #nolint
  map <- toolGetMapping("prom-gem-mappingNEW.csv", type = "sectoral", where = "mrprom") # nolint
  map <- filter(map, map[["PROM.Code"]] != "")
  tmp <- as.quitte(x[, , "Unit Cost"][, , map[["GEME3.Name"]]] * x[, , "Production Level"][, , map[["GEME3.Name"]]]) %>% # nolint
    interpolate_missing_periods(period = seq(2010, 2100, 1), expand.values = TRUE) %>%
    as.magpie() %>% # nolint
    collapseNames() # nolint


  # For HOU (PROM sector) use from GEME3: SUM(GEME3_SECTORS, P_HC * A_HC)
  # FIXME, some GEME3 countries have data also for years after 2014, for these countries there is no need to filter data with 2014
  tmp2 <- as.quitte(dimSums(x[, 2014, "Household Consumption"] * x[, 2014, "End-Use Price (Consumption Products)"], na.rm = TRUE)) %>% # nolint
    interpolate_missing_periods(period = seq(2010, 2100, 1), expand.values = TRUE) %>%
    as.magpie() %>% # nolint
    collapseNames() %>% # nolint
    magclass::setNames(nm = "HOU") # nolint

  # TODO for all values after 2015, compute Production Level (and A_PC) growth rates
  # and apply to 2014 PL*UC (and P_HC * A_PC) value

  # aggregate to OPEN-PROM sectors (from GEM sectors)
  rel <- select(map, c("GEME3.Name", "PROM.Code")) # gem-prom sectoral mapping
  tmp <- toolAggregate(tmp, rel = rel, weight = NULL, from = "GEME3.Name", to = "PROM.Code", dim = 3) # nolint
  x <- mbind(tmp, tmp2)

  # TODO add BU&NEN (= PCH?)
  x <- add_columns(x, addnm = c("BU", "NEN", "PCH"), dim = 3.1)
  x[, , c("BU", "NEN", "PCH")] <- x[, , "CH"] # TODO: equal to PCH, not CH!
  getSets(x)[3] <- "variable"

  # add units
  x <- add_dimension(x, dim = 3.2, nm = "billion US$2014", add = "unit")

  # add transport
  period <- NULL
  pc <- as.quitte(readSource("IRF", subtype = "passenger-cars-in-use")) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE))
  pc[["value"]] <- pc[["value"]] / 10^6
  pc[["unit"]] <- paste0("million ", pc[["unit"]])
  pb <- as.quitte(readSource("IRF", subtype = "buses-and-motor-coaches-in-use")) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE))
  pb[["value"]] <- pb[["value"]] / 10^9
  pb[["unit"]] <- "Billion pKm/yr"
  #    pc <- pc[intersect(getRegions(x), getRegions(pc)), intersect(getYears(x), getYears(pc)), ] / 10^6
  pt <- as.quitte(readSource("IRF", subtype = "inland-surface-passenger-transport-by-rail")) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE))
  pt[["value"]] <- pt[["value"]] / 1000
  pt[["unit"]] <- "Billion pKm/yr"
  #    pt <- pt[intersect(getRegions(pc), getRegions(pt)), intersect(getYears(pc), getYears(pt)), ]
  pa <- as.quitte(readSource("WDI_PA", convert = TRUE)) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE))
  pa[["value"]] <- pa[["value"]] / 1e+06 #million passengers
  #    pa <- pa[intersect(getRegions(pt), getRegions(pa)), intersect(getYears(pt), getYears(pa)), ]
  gu <- as.quitte(readSource("IRF", subtype = "inland-surface-freight-transport-by-road")) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE))
  gu[["value"]] <- gu[["value"]] / 1000
  gu[["unit"]] <- "GtKm/yr"
  #    gu <- gu[intersect(getRegions(pa), getRegions(gu)), intersect(getYears(pa), getYears(gu)), ]
  gt <- as.quitte(readSource("IRF", subtype = "inland-surface-freight-transport-by-rail")) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE))
  gt[["value"]] <- gt[["value"]] / 1000
  gt[["unit"]] <- "GtKm/yr"
  #    gt <- gt[intersect(getRegions(gu), getRegions(gt)), intersect(getYears(gu), getYears(gt)), ]
  gn <- as.quitte(readSource("IRF", subtype = "inland-surface-freight-transport-by-inland-waterway")) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE))
  gn[["value"]] <- gn[["value"]] / 1000
  gn[["unit"]] <- "GtKm/yr"
  pn <- readSource("TREMOVE", subtype = "Stock")
  pn <- pn[,,"REF"][,,"NAVIGATION"][,,"Passenger"]
  pn <- dimSums(pn[,,"Passenger"],3)
  pn <- toolCountryFill(pn, fill = NA)
  pn <- as.quitte(pn) %>%
    interpolate_missing_periods(period = getYears(pn, as.integer = TRUE)[1] : last(getYears(pn, as.integer = TRUE)), expand.values = TRUE)
  pn <- pn %>% filter(`period` %in% getYears(x, as.integer = TRUE))
  pn[["value"]] <- pn[["value"]] / 10^6
  pn[["unit"]] <- "Billion pKm/yr"
  pn[["variable"]] <- "inland-surface-passenger-transport-by-inland-waterway"
  #    gn <- gn[intersect(getRegions(gt), getRegions(gn)), intersect(getYears(gt), getYears(gn)), ]
  #    pc <- pc[intersect(getRegions(gn), getRegions(pc)), intersect(getYears(gn), getYears(pc)), ]
  #    pt <- pt[intersect(getRegions(pc), getRegions(pt)), intersect(getYears(pc), getYears(pt)), ]
  tr <- rbind(pc, pt, pa, gu, gt, gn, pn, pb)
  #    x <- mbind(x, mbind(tr, new.magpie(getRegions(tr), setdiff(getYears(x), getYears(tr)), getNames(tr), fill = NA)))
  levels(tr[["variable"]]) <- sub("passenger-cars-in-use", "PC", levels(tr[["variable"]]))
  levels(tr[["variable"]]) <- sub("inland-surface-passenger-transport-by-rail", "PT", levels(tr[["variable"]]))
  levels(tr[["variable"]]) <- sub("Air transport, passengers carried", "PA", levels(tr[["variable"]]))
  levels(tr[["variable"]]) <- sub("inland-surface-freight-transport-by-road", "GU", levels(tr[["variable"]]))
  levels(tr[["variable"]]) <- sub("inland-surface-freight-transport-by-rail", "GT", levels(tr[["variable"]]))
  levels(tr[["variable"]]) <- sub("inland-surface-freight-transport-by-inland-waterway", "GN", levels(tr[["variable"]])) # nolint
  levels(tr[["variable"]]) <- sub("inland-surface-passenger-transport-by-inland-waterway", "PN", levels(tr[["variable"]]))
  levels(tr[["variable"]]) <- sub("buses-and-motor-coaches-in-use", "PB", levels(tr[["variable"]])) # nolint
  qx <- rbind(as.quitte(x), filter(tr, tr[["region"]] %in% getRegions(x)))

  # assign to countries with NA, their H12 region mean
  qx_bu <- qx
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

  #getNames(x) <- sub("\\..*$", "", getNames(x))

  list(x = x,
       weight = NULL,
       unit = "various",
       description = "economic activity data for OPENPROM sectors")
}
