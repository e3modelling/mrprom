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

  country = "Bangladesh"
  countryCode = "BGD"

  # total population and growth from UN
  x <- readSource("UNPopDiv")
  pop <- x[country,2010:2050,]
  getItems(pop,1) <- countryCode

  # GDP and growth from World-Bank, IMF for short projections and SSP2 for long term projections
  x1 <- readSource("WorldBankWDI")
  a1 <- x1[country,2010:2024,"GDP (constant 2015 US$)"]
  getItems(a1,1) <- countryCode

  x2 <- readSource("IMF", convert = TRUE)
  a2 <- x2[countryCode,2025:2030,"Gross domestic product (GDP), Constant prices, Domestic currency.Domestic currency"]
  getItems(a2,3) <- "GDP (constant 2015 US$)"
  a2 <- a2*10^7 # Convert from tenth millions dollars to dollars

  x3 <- calcOutput("iGDP", aggregate = FALSE)
  a3 <- x3[countryCode,2030:2050,]
  # Convert from PPP to MER with conversion factor from World Bank. Choose monetary year
  PPPtoMER <- x1[country,2015,"Price level ratio of PPP conversion factor (GDP) to market exchange rate"][1]
  a3 <- a3 * PPPtoMER
  a3 <- a3 * 10^9 #  Convert from Billions dollars to dollars
  # Harmonization via Scaling
  # Apply a scaling factor to all SSP2 values from 2030 onward
  scalingFactor <- a2[countryCode,2030,][1]/a3[countryCode,2030,][1]
  a4 <- a3 * scalingFactor
  getItems(a4,3) <- "GDP (constant 2015 US$)"

  GDP <- Reduce(mbind, list(a1, a2, a4[,2031:2050,]))

  # energy demand data - ENERDATA until 2021
  # shares
  x <- readSource("ENERDATA", subtype =  "Share", convert = TRUE)
  sharesResidential <- getData("Shares","households", countryCode, x)
  sharesAgriculture <- getData("Shares","agriculture", countryCode, x)
  sharesServices <- getData("Shares","services", countryCode, x)
  sharesIndustry <- getData("Shares","industry", countryCode, x)
  sharesTransport <- getData("Shares","transports", countryCode, x)  
  sharesNonEnergyUses <- getData("Shares","non energy industries", countryCode, x)

  shares <- Reduce(mbind, list(sharesResidential,sharesAgriculture, sharesServices,
              sharesIndustry,sharesTransport,sharesNonEnergyUses))

  # total values
  x <- readSource("ENERDATA", subtype =  "energy final consumption", convert = TRUE)
  consumptionAgriculture <- getTotal("agriculture", countryCode, x)
  consumptionIndustry <- getTotal("industry", countryCode, x)
  consumptionTransport <- getTotal("transport", countryCode, x)
  consumptionNonEnergyUses <- getTotal("non energy uses", countryCode, x) 
  consumptionServices <- x[countryCode,2010:2021,"Total energy final consumption of tertiary sector.Mtoe"]

  # specific industry
  consumptionNMMI <- getTotal("non-metallic minerals", countryCode, x)
  consumptionMI <- x[countryCode, 2010:2021, "Total energy final consumption miscellaneous industry"]

  # extract residential demand
  x <- readSource("ENERDATA", subtype =  "residential", convert = TRUE)
  consumptionResidential  <- x[countryCode,2010:2021,"Total final energy consumption of residential sector.Mtoe"]

  consumption <- Reduce(mbind, list(consumptionAgriculture, consumptionServices, consumptionIndustry, consumptionTransport, 
                        consumptionNonEnergyUses,consumptionResidential,consumptionNMMI,consumptionMI))

  # find fuels per category
  x <- readSource("ENERDATA", subtype =  "non-metallic minerals", convert = TRUE)
  fuelConsumptionNMMI <- getData("dummy","non-metallic minerals", countryCode, x)
  x <- readSource("ENERDATA", subtype =  "miscellaneous industry", convert = TRUE)
  fuelConsumptionMI <- getData("dummy","miscellaneous industry", countryCode, x)

  consumption <- Reduce(mbind, list(consumption,fuelConsumptionNMMI, fuelConsumptionMI))

  # activity - used code lines from calcACTV 
  x <- readSource("GEME3", convert = TRUE) #nolint
  map <- toolGetMapping("prom-gem-mappingNEW.csv", type = "sectoral", where = "mrprom") # nolint
  map <- filter(map, map[["PROM.Code"]] != "")
  tmp <- as.quitte(x[, , "Unit Cost"][, , map[["GEME3.Name"]]] * x[, , "Production Level"][, , map[["GEME3.Name"]]]) %>% # nolint
    interpolate_missing_periods(period = seq(2010, 2100, 1), expand.values = TRUE) %>%
    as.magpie() %>% # nolint
    collapseNames() # nolint

  tmp2 <- as.quitte(dimSums(x[, 2014, "Household Consumption"] * x[, 2014, "End-Use Price (Consumption Products)"], na.rm = TRUE)) %>% # nolint
    interpolate_missing_periods(period = seq(2010, 2100, 1), expand.values = TRUE) %>%
    as.magpie() %>% # nolint
    collapseNames() %>% # nolint
    magclass::setNames(nm = "HOU") # nolint

  # aggregate to OPEN-PROM sectors (from GEM sectors)
  rel <- select(map, c("GEME3.Name", "PROM.Code")) # gem-prom sectoral mapping
  tmp <- toolAggregate(tmp, rel = rel, weight = NULL, from = "GEME3.Name", to = "PROM.Code", dim = 3) # nolint
  x <- mbind(tmp, tmp2)

  a <- x[countryCode,2010:2050,]
  # convert from PPP to MER 
  a <- a * PPPtoMER
  
  # Separate activies from BM and all other to match energy consumptions from ENERDATA
  allIndustry <- a[, , c("OE", "IS", "NF", "CH", "PP", "BM", "EN", "OI", "TX", "FD")]
  sumIndustry <- dimSums(allIndustry, 3)
  getNames(sumIndustry) <- "TotalINDSE"
  otherIndustry <- sumIndustry - a[, , "BM"] 
  getNames(otherIndustry) <- "OtherINDSE"

  allIndustry <- Reduce(mbind, list(sumIndustry, otherIndustry))

  activity <- mbind(a, allIndustry)

  # Export to LEAP template 

  # Combine magpies to a joint dataframe
dfPop <- magpieToLong(pop)
dfShares <- magpieToLong(shares)
dfGDP <- magpieToLong(GDP)
dfConsumption <- magpieToLong(consumption)
dfActivity <- magpieToLong(activity)

combinedMagpie <- bind_rows(dfPop, dfGDP, dfShares, dfConsumption, dfActivity)  %>%
  mutate(Scenario = "Default")  # adjust if you have real scenario info

colnames(combinedMagpie)[colnames(combinedMagpie) == "Data1"] <- "Variable"

# Read the mapping CSV
mapping <- read.csv("prom-leap-mapping.csv",
                    stringsAsFactors = FALSE,
                    na.strings = c("NA", ""))
                      # map <- toolGetMapping(name = "prom-leap-mapping.csv",
                      #   type = "sectoral",
                      #   where = "mrprom")

# Ensure the ID columns are integer
mapping <- mapping %>%
  mutate(across(c(branchId, variableId, scenarioId, regionId),
                ~ as.integer(.)))

# Check that all column names are the same, e.g. uppercase
# print("combinedMagpie columns:")
# print(names(combinedMagpie))

# print("mapping columns:")
# print(names(mapping))

updatesDf <- combinedMagpie %>%
  inner_join(mapping,
             by = c("Scenario", "Region", "Variable")) %>%
  select(branchId, variableId, scenarioId, regionId, Year, Value)

# Code snippet for tracking duplicates in a dataframe
#   updatesDf |>
#   dplyr::summarise(n = dplyr::n(), .by = c(branchId, variableId,     
#   scenarioId, regionId, Year)) |>
#   dplyr::filter(n > 1L)

updateLeapTemplateByIds(
  templatePath = "template.xlsx",
  outputPath = "LeapImportData.xlsx",
  sheetName = "Export",
  updatesDf = updatesDf
)
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