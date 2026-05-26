#' calcIFullACTV
#'
#' Derive economic activity data for OPENPROM sectors based on two data sources:
#' transport, traffic, air transport passengers per country and per year (IRF)
#' and Production Level and Unit Cost (GEME3).
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IFullACTV", aggregate = TRUE)
#' }
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr filter select last group_by mutate

calcIFullACTV <- function() {
  
  x <- readSource("GEME3", convert = TRUE)
  x <- x[, getYears(x) != "y2016", ]
  y <- x
  # Reading Inputs
  ProductionLevel <- y[, , "Production Level"]
  Exports <- y[, , "Total Exports"]
  UnitCost <- y[, , "Unit Cost"]
  HouseholdConsumption <- y[, , "Household Consumption"]
  EndPrices <- y[, , "End-Use Prices"]
  ExportPrices <- y[, , "Unit Cost Exports"]
  ActivityExports <- y[, , "Activity Exports"]
  
  # All prices should be baseyear (2017) prices
  UnitCost2017 <- UnitCost[, "y2017", , drop = FALSE]
  EndPrices2017 <- EndPrices[, "y2017", , drop = FALSE]
  ExportPrices2017 <- ExportPrices[, "y2017", , drop = FALSE]
  
  # Convert volumes to baseyear values
  # Production
  ProductionValue <- ProductionLevel * UnitCost2017
  years_current <- getYears(ProductionValue)
  years_clean <- sub("\\..*$", "", years_current)
  getYears(ProductionValue) <- years_clean
  ProductionValue <- collapseNames(ProductionValue, collapsedim = 3)
  getNames(ProductionValue, dim = "variable") <- paste0("Production Value")
  # Households
  HouseholdConsumptionVal <- HouseholdConsumption * EndPrices2017
  getYears(HouseholdConsumptionVal) <- years_clean
  HouseholdConsumptionVal <- collapseNames(HouseholdConsumptionVal, collapsedim = 3)
  getNames(HouseholdConsumptionVal, dim = "variable") <- paste0("HouseholdConsumptionVal")
  # Exports
  missingExports <- paste0("Total Exports.", setdiff(getItems(x, 3.2), getItems(Exports, 3.2)))
  tmp <- Exports[, , 1, drop = FALSE]
  tmp[,] <- 0
  tmp_list <- lapply(missingExports, function(v){
    y <- tmp
    getNames(y) <- v
    return(y)
  })
  new_sectors <- do.call(mbind, tmp_list)
  Exports <- mbind(Exports, new_sectors)
  ExportsValue <- (Exports + ActivityExports) * ExportPrices2017[, , getItems(Exports, 3.2)]
  getYears(ExportsValue) <- years_clean
  ExportsValue <- collapseNames(ExportsValue, collapsedim = 3)
  getNames(ExportsValue, dim = "variable") <- paste0("ExportsValue")
  
  # Sectoral mapping
  map <- toolGetMapping("prom_geme3_map.csv", type = "sectoral", where = "mrprom")
  
  map <- filter(map, map[["PROM.Code"]] != "")
  
  ProductionVal <- as.quitte(ProductionValue[, , unique(map[["GEME3.Name"]])]) %>%
    interpolate_missing_periods(period = seq(2010, 2100, 1), expand.values = TRUE) %>%
    as.magpie() %>%
    collapseNames()
  
  
  # For HOU (PROM sector) use from GEME3: SUM(GEME3_SECTORS, HouseholdConsumptionVal = P_HC * A_HC)
  Households <- as.quitte(dimSums(HouseholdConsumptionVal, dim = 3.2, na.rm = TRUE)) %>% 
    interpolate_missing_periods(period = seq(2010, 2100, 1), expand.values = TRUE) %>%
    as.magpie() %>%
    collapseNames() %>%
    magclass::setNames(nm = "HOU")
  # For BU (PROM sector) use from GEME3: SUM(GEME3_REGIONS, SUM(GEME3_SECTORS, ExportsValue = (A_YVTWR + A_EXPOT) * P_PWE ))
  Bunkers <- as.quitte(dimSums(ExportsValue, dim = c(1, 3.2), na.rm = TRUE)) %>% 
    interpolate_missing_periods(period = seq(2010, 2100, 1), expand.values = TRUE) %>%
    as.magpie() %>%
    collapseNames() %>%
    magclass::setNames(nm = "BU")
  # Common growth rate for Bunkers for all regions
  regions <- getRegions(x)
  BunkersAll <- new.magpie(cells_and_regions = regions, years = getYears(Bunkers))
  BunkersAll[, , ] <- Bunkers
  getNames(BunkersAll) <- "BU"
  
  # aggregate to OPEN-PROM sectors (from GEM sectors)
  rel <- select(map, c("GEME3.Name", "PROM.Code")) # gem-prom sectoral mapping
  
  ProductionVal <- toolAggregate(ProductionVal, rel = rel, weight = NULL, from = "GEME3.Name", to = "PROM.Code", dim = 3) # nolint
  x <- mbind(ProductionVal, Households, BunkersAll)
  
  # Transport Activity needs to be checked
  # add transport
  period <- NULL
  pc <- as.quitte(readSource("IRF", subtype = "passenger-cars-in-use")) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE)) %>%
    mutate(
      value = value / 1e6,
      unit = paste0("million ", unit)
    )
  
  # 55,000 km/year each bus
  # 18 passengers average
  
  pb <- as.quitte(readSource("IRF", subtype = "buses-and-motor-coaches-in-use") * 55000 * 18) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE)) %>%
    mutate(
      value = value / 1e9,
      unit = "Billion pKm/yr"
    )
  
  #    pc <- pc[intersect(getRegions(x), getRegions(pc)), intersect(getYears(x), getYears(pc)), ] / 10^6
  pt <- as.quitte(readSource("IRF", subtype = "inland-surface-passenger-transport-by-rail")) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE)) %>%
    mutate(
      value = value / 1e3,
      unit = "Billion pKm/yr"
    )
  
  pa <- as.quitte(readSource("WDI_PA", convert = TRUE)) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE)) %>%
    mutate(
      value = value / 1e6
    ) #million passengers
  pa[["variable"]] <- "Air transport, million passengers"
  pa[["unit"]] <- "million passengers"
  
  
  #    pa <- pa[intersect(getRegions(pt), getRegions(pa)), intersect(getYears(pt), getYears(pa)), ]
  gu <- as.quitte(readSource("IRF", subtype = "inland-surface-freight-transport-by-road")) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE)) %>%
    mutate(
      value = value / 1e3,
      unit = "GtKm/yr"
    )
  
  #    gu <- gu[intersect(getRegions(pa), getRegions(gu)), intersect(getYears(pa), getYears(gu)), ]
  gt <- as.quitte(readSource("IRF", subtype = "inland-surface-freight-transport-by-rail")) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE)) %>%
    mutate(
      value = value / 1e3,
      unit = "GtKm/yr"
    )
  
  
  gn <- as.quitte(readSource("IRF", subtype = "inland-surface-freight-transport-by-inland-waterway")) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE)) %>%
    mutate(
      value = value / 1e3,
      unit = "GtKm/yr"
    )
  
  pn <- readSource("TREMOVE", subtype = "Stock")
  pn <- pn[,,"REF"][,,"NAVIGATION"][,,"Passenger"]
  pn <- dimSums(pn[,,"Passenger"],3)
  
  suppressMessages(
    suppressWarnings(
      pn <- toolCountryFill(pn, fill = NA)
    )
  )
  
  pn <- as.quitte(pn) %>%
    interpolate_missing_periods(period = getYears(pn, as.integer = TRUE)[1] : last(getYears(pn, as.integer = TRUE)), expand.values = TRUE)
  
  pn <- pn %>%
    filter(`period` %in% getYears(x, as.integer = TRUE)) %>%
    mutate(
      value = value / 1e6,
      unit = "Billion pKm/yr",
      variable = "inland-surface-passenger-transport-by-inland-waterway"
    )
  
  #    gn <- gn[intersect(getRegions(gt), getRegions(gn)), intersect(getYears(gt), getYears(gn)), ]
  #    pc <- pc[intersect(getRegions(gn), getRegions(pc)), intersect(getYears(gn), getYears(pc)), ]
  #    pt <- pt[intersect(getRegions(pc), getRegions(pt)), intersect(getYears(pc), getYears(pt)), ]
  tr <- rbind(pc, pt, pa, gu, gt, gn, pn, pb)
  #    x <- mbind(x, mbind(tr, new.magpie(getRegions(tr), setdiff(getYears(x), getYears(tr)), getNames(tr), fill = NA)))
  levels(tr[["variable"]]) <- sub("passenger-cars-in-use", "PC", levels(tr[["variable"]]))
  levels(tr[["variable"]]) <- sub("inland-surface-passenger-transport-by-rail", "PT", levels(tr[["variable"]]))
  levels(tr[["variable"]]) <- sub("Air transport, million passengers", "PA", levels(tr[["variable"]]))
  levels(tr[["variable"]]) <- sub("inland-surface-freight-transport-by-road", "GU", levels(tr[["variable"]]))
  levels(tr[["variable"]]) <- sub("inland-surface-freight-transport-by-rail", "GT", levels(tr[["variable"]]))
  levels(tr[["variable"]]) <- sub("inland-surface-freight-transport-by-inland-waterway", "GN", levels(tr[["variable"]])) # nolint
  levels(tr[["variable"]]) <- sub("inland-surface-passenger-transport-by-inland-waterway", "PN", levels(tr[["variable"]]))
  levels(tr[["variable"]]) <- sub("buses-and-motor-coaches-in-use", "PB", levels(tr[["variable"]])) # nolint
  names(dimnames(x))[3] <- "variable"
  qx <- rbind(as.quitte(x), filter(tr, tr[["region"]] %in% getRegions(x)))
  x <- qx %>%
    replace_na(list(value = 0)) %>%
    as.quitte() %>%
    as.magpie()
  
  
  list(x = x,
       weight = NULL,
       unit = "various",
       description = "economic activity data for OPEN-PROM sectors")
}