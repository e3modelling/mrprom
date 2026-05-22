#' fullLEAP
#' 
#' Read in several files with data from UN world prospects, IMF, GEM-E3, Enerdata and convert it
#' to an xlsx file.
#' 
#' @return The read-in target data into a magpie object.
#'
#' @author Alexandros Tsimpoukis, Anastasis Giannousakis
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr select %>%
#' @importFrom tidyr pivot_wider
#' @import  openxlsx
#' @examples
#' \dontrun{
#' a <- retrieveData("LEAP" )
#' }
fullLEAP <- function() {

  country <- "Kazakhstan"
  countryCode <- "KAZ"

  # total population and growth from UN
  x <- readSource("UNPopDiv")
  pop <- x[country, 2010:2050, "totalPop"]/1000 # convert from thousands to millions
  getItems(pop,1) <- countryCode
  pop <- add_dimension(pop, dim = 3.2, add = "unit", nm = "million people")
  
  # GDP and growth from World-Bank, IMF for short projections and SSP2 for long term projections
  # 1. World Bank (historical)
  worldBankData <- readSource("WorldBankWDI")
  gdpHist <- worldBankData[country, 2010:2024, "GDP (constant 2015 US$)"]
  getItems(gdpHist, 1) <- countryCode
  getItems(gdpHist, 3) <- "GDP"
  # 2. IMF (proper conversion to constant 2015 USD)
  imfData <- readSource("IMF", convert = TRUE)
  # Real GDP (constant prices, domestic currency, base 2005)
  gdpImfRaw <- imfData[countryCode, 2025:2030,
    "Gross domestic product (GDP), Constant prices, Domestic currency"]
  # GDP deflator values (index)
  deflator2015 <- worldBankData[country, 2015,
    "GDP deflator (base year varies by country)"][1]
  deflator2005 <- worldBankData[country, 2005,
    "GDP deflator (base year varies by country)"][1]
  rebasingFactor <- deflator2015 / deflator2005
  # Exchange rate (LCU per USD, 2015)
  exchangeRate2015 <- worldBankData[country, 2015,
    "Official exchange rate (LCU per US$, period average)"][1]

  # Step 1: rebase to constant 2015 LCU
  gdpImf2015Lcu <- gdpImfRaw * rebasingFactor
  # Step 2: convert to USD
  gdpImf <- gdpImf2015Lcu / exchangeRate2015
  getItems(gdpImf, 3) <- "GDP"
  gdpImf <- gdpImf * 10^9 # convert from billions to dollars

  # 3. SSP2 projections
  sspData <- calcOutput("iGDP", aggregate = FALSE)
  gdpSsp <- sspData[countryCode, 2030:2050, ]
  pppConv <- worldBankData[country, 2015,
    "PPP conversion factor, GDP (LCU per international $)"][1]
  pppToMer <- pppConv / exchangeRate2015
  gdpSsp <- gdpSsp * pppToMer
  # Convert from billions to dollars
  gdpSsp <- gdpSsp * 10^9
  # 4. Harmonization
  scalingFactor <- gdpImf[countryCode, 2030, ][1] / gdpSsp[countryCode, 2030, ][1]
  gdpSspScaled <- gdpSsp * scalingFactor

  getItems(gdpSspScaled, 3) <- "GDP"

  GDP <- mbind(gdpHist, gdpImf, gdpSspScaled[, 2031:2050, ])
  GDP <- add_dimension(GDP, dim = 3.2, add = "unit", nm = "US$") # GDP in constant 2015 USD

  # activities
  
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

  x <- add_dimension(x, dim = 3.2, nm = "US$", add = "unit")
  
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
  names(dimnames(x))[3] <- "variable.unit"
  qx <- rbind(as.quitte(x), filter(tr, tr[["region"]] %in% getRegions(x)))
  x <- qx %>%
    replace_na(list(value = 0)) %>%
    as.quitte() %>%
    as.magpie()

  act <- x[countryCode, , ]

  x <- calcOutput(type = "IFuelCons2", aggregate = FALSE)

  fuelCons <- x[countryCode, , ]
  fuelConsLong <- as.quitte(fuelCons) %>%
    select(c("region", "dsbs", "ef", "period", "value"))

  fuelConsq <- fuelConsLong %>%
    pivot_wider(names_from = "period")
  fheader <- paste("Country,Sector,Fuel", paste(colnames(fuelConsq)[4:length(colnames(fuelConsq))], collapse = ","), sep = ",")
  writeLines(fheader, con = paste0("LEAP_EnergyDemand.csv"))
  write.table(fuelConsq,
    quote = FALSE,
    row.names = FALSE,
    file = paste0("LEAP_EnergyDemand.csv"),
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  fuelSharesq <- fuelConsLong %>%
    group_by(region, dsbs, period) %>%
    mutate(
      sectorTotal = sum(value, na.rm = TRUE),
      value = ifelse(sectorTotal == 0, 0, round(value / sectorTotal * 100, 6))
    ) %>%
    ungroup() %>%
    select(c("region", "dsbs", "ef", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("Country,Sector,Fuel", paste(colnames(fuelSharesq)[4:length(colnames(fuelSharesq))], collapse = ","), sep = ",")
  writeLines(fheader, con = paste0("LEAP_EnergyDemandShares.csv"))
  write.table(fuelSharesq,
    quote = FALSE,
    row.names = FALSE,
    file = paste0("LEAP_EnergyDemandShares.csv"),
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )


  keyAssumptions <- mbind(pop, GDP, act[,2010:2050,])
  write.report(keyAssumptions, file = "LEAP_keyAssumptions.csv")

}

# Helpers ------------------------------------------------
getData <- function(type=NULL, sector, countryCode, x,
                          years = 2010:2021,
                          fuels = c("Coal and lignite","coal and lignite", "oil", "Oil","Natural gas","gas",
                                    "electricity", "Electricity","heat","Heat","biomass","Biomass"),
                          warnMissing = TRUE) {
  # build expected item names
  if (type=="Shares") {
    expectedItems <- paste0("Share of ", fuels, " in ", sector, " consumption.%")
    expectedItemsNoOf <- paste0(fuels," final consumption ", sector, ".Mtoe")
  } else {
    expectedItems <- paste0(fuels," final consumption of ", sector, ".Mtoe")
    expectedItemsNoOf <- paste0(fuels," final consumption ", sector, ".Mtoe")
  }
  regions <- as.character(countryCode)

  # what items actually exist in x?
  actualItems <- getItems(x,3)
  presentItems <- intersect(expectedItems, actualItems)
  presentItemsNoOf <- intersect(expectedItemsNoOf, actualItems)
  missingItems <- setdiff(expectedItems, actualItems)

  if (length(missingItems) && warnMissing) {
    warning("Missing columns: ", paste(missingItems, collapse = ", "))
  }

  # subset what exists (could be empty)
  if (length(presentItems)) {
    presentSliceWithOf <- x[regions, years, presentItems, drop = FALSE]
    presentSliceNoOf <- x[regions, years, presentItemsNoOf, drop = FALSE]
    presentSlice <- mbind(presentSliceWithOf,presentSliceNoOf)

  } else {
    # create an empty magpie slice with zero items
    emptyArray <- array(NA_real_,
                        dim = c(length(regions), length(years), 0),
                        dimnames = list(regions, years, character(0)))
    presentSlice <- as.magpie(emptyArray)
  }
}

getTotal <- function(sector, countryCode, x) {
  total <- paste0("Total energy final consumption of ", sector, ".Mtoe")
  x[countryCode, 2010:2021, total]
}

# Helper: normalize column matching (case/whitespace-insensitive)
findColIgnoreCase <- function(dfNames, target) {
  # Returns the first name in dfNames whose trimmed lower equals target lower
  cleaned <- trimws(dfNames)
  idx <- which(tolower(cleaned) == tolower(target))
  if (length(idx) >= 1) return(dfNames[idx[1]])
  return(NA_character_)
}

# Uses the four LEAP ID columns to apply per-year value updates
updateLeapTemplateByIds <- function(templatePath,
                                    outputPath,
                                    sheetName = "Export",   # typical sheet name by LEAP
                                    updatesDf) {

  # Expect updatesDf with columns: branchId, variableId, scenarioId, regionId, year, value
  requiredUpdateCols <- c("branchId", "variableId", "scenarioId", "regionId", "Year", "Value")
  if (!all(requiredUpdateCols %in% names(updatesDf))) {
    stop("updatesDf must contain columns: ", paste(requiredUpdateCols, collapse = ", "),
         ". Provided columns: ", paste(names(updatesDf), collapse = ", "))
  }
  # Coerce key columns to character so joins don't fail due to type mismatch
  updatesDf <- updatesDf %>%
    mutate(
      branchId = as.character(branchId),
      variableId = as.character(variableId),
      scenarioId = as.character(scenarioId),
      regionId = as.character(regionId),
      Year = as.character(Year)
    )
  
  wb <- loadWorkbook(templatePath)
  
  templateDf <- read.xlsx(templatePath, sheet = sheetName, startRow = 3, colNames = TRUE)
  originalNames <- names(templateDf)
  cat("Detected template column names:\n")
  print(originalNames)
  
  # Locate the ID columns
  colBranch <- findColIgnoreCase(originalNames, "BranchID")
  colVariable <- findColIgnoreCase(originalNames, "VariableID")
  colScenario <- findColIgnoreCase(originalNames, "ScenarioID")
  colRegion <- findColIgnoreCase(originalNames, "RegionID")
  
  missingIds <- c()
  if (is.na(colBranch)) missingIds <- c(missingIds, "BranchID")
  if (is.na(colVariable)) missingIds <- c(missingIds, "VariableID")
  if (is.na(colScenario)) missingIds <- c(missingIds, "ScenarioID")
  if (is.na(colRegion)) missingIds <- c(missingIds, "RegionID")
  if (length(missingIds) > 0) {
    stop("Template sheet is missing expected ID columns (case/whitespace insensitive): ",
         paste(missingIds, collapse = ", "), 
         ". Available columns: ", paste(originalNames, collapse = ", "))
  }
  
  # Standardize by creating helper join keys
  templateDf <- templateDf %>%
    mutate(
      branchId = as.character(.data[[colBranch]]),
      variableId = as.character(.data[[colVariable]]),
      scenarioId = as.character(.data[[colScenario]]),
      regionId = as.character(.data[[colRegion]])
    )
  
  # Detect year columns (exact four-digit names)
  yearCols <- grep("^[0-9]{4}$", originalNames, value = TRUE)
  if (length(yearCols) == 0) {
    stop("No year columns detected in template. Expected columns named like '2010', '2011', etc.")
  }
  cat("Detected year columns to potentially update:", paste(yearCols, collapse = ", "), "\n")
  
  wideUpdates <- updatesDf %>%
  mutate(across(c(branchId, variableId, scenarioId, regionId, Year), as.character)) %>%
  pivot_wider(names_from = Year, values_from = Value)
  
    # Assume templateDf has been read and wideUpdates is the pivoted updates with years as columns
    # After left_join with suffix to separate original vs update:
    mergedDf <- templateDf %>%
    left_join(wideUpdates, by = c("branchId", "variableId", "scenarioId", "regionId"),
                suffix = c("", "_new"))

    # Identify year columns (e.g., "2010", "2011", etc.)
    yearCols <- grep("^[0-9]{4}$", names(templateDf), value = TRUE)

    # For each year, if the corresponding updated column (year_new) is present and non-NA, overwrite
    for (yr in yearCols) {
    updatedCol <- paste0(yr, "_new")
    if (updatedCol %in% names(mergedDf)) {
        mergedDf[[yr]] <- ifelse(!is.na(mergedDf[[updatedCol]]), mergedDf[[updatedCol]], mergedDf[[yr]])
    }
    }

    # Drop the helper and _new columns before writing back
    cleanedDf <- mergedDf %>%
    select(-any_of(c("branchId", "variableId", "scenarioId", "regionId",
                    paste0(yearCols, "_new"))))
  
  writeData(wb, sheet = sheetName, cleanedDf, startRow = 4, colNames = FALSE, withFilter = FALSE, na.string   = "" )
  saveWorkbook(wb, outputPath, overwrite = TRUE)
  message("Updated LEAP import file written to: ", outputPath)
}


magpieToLong <- function(m) {

  if (!inherits(m, "magpie")) stop("Object is not a magpie object")
  dims <- names(dimnames(m))
  dn <- dimnames(m)

  # Identify year-like, region-like, and variable-like dimension names
  yearDim <- grep("^(Year|year|period)$", dims, ignore.case = TRUE, value = TRUE)[1]
  regionDim <- grep("^(Location|location|country|region)$", dims, ignore.case = TRUE, value = TRUE)[1]
  variableDim <- setdiff(dims, c(yearDim, regionDim))[1]

  if (is.null(yearDim) || is.null(regionDim) || is.null(variableDim)) {
    stop("Unable to infer dimensions from magpie object. Found dims: ", paste(dims, collapse = ", "))
  }

  df <- as.data.frame(as.magpie(m))

  # Rename to consistent names
  colnames(df)[colnames(df) == regionDim] <- "region"
  colnames(df)[colnames(df) == yearDim] <- "Year"
  colnames(df)[colnames(df) == variableDim] <- "variable"

  return(df)
}
