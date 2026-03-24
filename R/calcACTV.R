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
#' a <- calcOutput(type = "ACTV", file = "iACTV.csvr", aggregate = TRUE)
#' }
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr filter select last group_by mutate

calcACTV <- function() {

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
  # map <- toolGetMapping("prom_geme3_map.csv", type = "sectoral", where = "mrprom")
  map <- read.csv("D:/mrprom/inst/extdata/sectoral/prom_geme3_map.csv")
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

  #period-to-period growth ratio
  growth <- as.quitte(x) %>% select(-c(variable)) %>% rename(variable = sector)
  growth <- growth %>%
    arrange(region, variable, period) %>%   # Sort by region, variable, and period
    group_by(region, variable) %>%          # Group by region and variable
    mutate(
      prev_value = lag(value),
      diff_ratio = value / if_else(prev_value == 0, 1, prev_value)
    ) %>%
    ungroup()
  
  growth <- select(growth, c("region","variable","unit","period","diff_ratio"))
  names(growth) <- sub("diff_ratio","value",names(growth))
  
  #average (2018–2030) if the period is before 2018
  df <- growth %>%
    group_by(region, variable) %>%
    mutate(
      value_2018_2030 = mean(value[period >= 2018 & period <= 2030], na.rm = TRUE),  # average of 2010–2017
      value = ifelse(period < 2018, value_2018_2030, value)
    ) %>%
    ungroup() %>% select(-value_2018_2030)
  #------------------------------------------------------
  # Restriction of BUNKERS growth rate (if value for BU is larger than 1.01 keep value 1.01)
  # df <- df %>%
  #  mutate(
  #    value = if_else(variable == "BU" & value >= 1.01,
  #                    1.01,
  #                    value)
  #  )
  x <- as.quitte(df) %>% as.magpie()
  # add units
  x <- add_dimension(x, dim = 3.2, nm = "%", add = "unit")
  # ---------------------------------------------------------------------------
  # Transport Activity needs to be checked
  # add transport
  period <- NULL
  pc <- as.quitte(readSource("IRF", subtype = "passenger-cars-in-use")) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE)) %>%
    mutate(
      value = value / 1e6,
      unit = paste0("million ", unit)
    )

  pb <- as.quitte(readSource("IRF", subtype = "inland-surface-public-passenger-transport-by-road")) %>%
    filter(`period` %in% getYears(x, as.integer = TRUE)) %>%
    mutate(
      value = value / 1e3,
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
  levels(tr[["variable"]]) <- sub("inland-surface-public-passenger-transport-by-road", "PB", levels(tr[["variable"]])) # nolint
  qx <- rbind(as.quitte(x), filter(tr, tr[["region"]] %in% getRegions(x)))
  x <- qx %>%
    replace_na(list(value = 0)) %>%
    as.quitte() %>%
    as.magpie()



  # # assign to countries with NA, their H12 region mean
  
  # qx_bu <- qx
  # h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  # names(qx) <- sub("region", "CountryCode", names(qx))
  # ## add h12 mapping to dataset
  # qx <- left_join(qx, h12, by = "CountryCode")
  # ## add new column containing regional mean value
  # value <- NULL
  # qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "variable"))
  # names(qx) <- sub("CountryCode", "region", names(qx))
  # qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  # qx_bu <- select(qx_bu, -c("model", "scenario"))
  # ## assign to countries with NA, their H12 region mean
  # value.x <- NULL
  # value.y <- NULL
  # qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
  #   mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
  #   select(-c("value.x", "value.y"))
  # ## assign to countries that still have NA, the global mean
  # qx_bu <- qx
  # qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "variable"))
  # qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
  #   mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
  #   select(-c("value.x", "value.y"))
  # x <- as.quitte(qx) %>% as.magpie()
  

  transport <- x[, , setdiff(getItems(x, 3.2),"%")]
  x <- x[, , "%"]
  x <- mbind(x, transport)
  # ------------------------------------------------------------------
  # Calculation of aggregation weights
  GDP <- calcOutput("iGDP", aggregate = FALSE) # will use gdp as disaggregation weights
  GDP <- GDP[, getYears(x), , drop = TRUE]

  Population <- calcOutput("POP", aggregate = FALSE)
  Population <- Population[, getYears(x), , drop = TRUE]

  GDPpCapita <- GDP / Population
  GDPpCapita[is.na(GDPpCapita)] <- 0
  weights <- x
  weights[, , ] <- GDP
  weights[, , c("PC", "PB", "PT", "PA", "PN", "GU", "GT", "GN")] <- NA
  weights[, , "HOU"] <- GDPpCapita

  list(x = x,
       weight = weights,
       unit = "various",
       description = "economic activity data for OPEN-PROM sectors",
       mixed_aggregation = TRUE)
}
