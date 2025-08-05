# Required packages (install if missing)
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")

library(openxlsx)
library(dplyr)
library(tidyr)

# Helper: normalize column matching (case/whitespace-insensitive)
findColIgnoreCase <- function(dfNames, target) {
  # Returns the first name in dfNames whose trimmed lower equals target lower
  cleaned <- trimws(dfNames)
  idx <- which(tolower(cleaned) == tolower(target))
  if (length(idx) >= 1) return(dfNames[idx[1]])
  return(NA_character_)
}

# Main updater: uses the four LEAP ID columns to apply per-year value updates
updateLeapTemplateByIds <- function(templatePath,
                                    outputPath,
                                    sheetName = "Export",
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
  
  # Write back into workbook starting at row 4 (data rows) without rewriting header
  writeData(wb, sheet = sheetName, cleanedDf, startRow = 4, colNames = FALSE, withFilter = FALSE,na.string   = "" )
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
  # The remaining dimension is treated as variable (could be variable.unit, sector, etc.)
  variableDim <- setdiff(dims, c(yearDim, regionDim))[1]

  if (is.null(yearDim) || is.null(regionDim) || is.null(variableDim)) {
    stop("Unable to infer dimensions from magpie object. Found dims: ", paste(dims, collapse = ", "))
  }

  # Use magclass as.data.frame to get long form
  df <- as.data.frame(as.magpie(m))

  # Rename to consistent names
  colnames(df)[colnames(df) == regionDim] <- "region"
  colnames(df)[colnames(df) == yearDim] <- "Year"
  colnames(df)[colnames(df) == variableDim] <- "variable"

  return(df)
}

# Combine magpies to a joint dataframe
dfPop <- magpieToLong(pop)
dfShares <- magpieToLong(shares)
dfGDP <- magpieToLong(GDP)
dfConsumption <- magpieToLong(consumption)
dfActivity <- magpieToLong(activity)

combinedMagpie <- bind_rows(dfPop, dfGDP, dfShares, dfConsumption, dfActivity)  %>%
  mutate(Scenario = "Default")  # adjust if you have real scenario info

colnames(combinedMagpie)[colnames(combinedMagpie) == "Data1"] <- "Variable"

write.csv(combinedMagpie,"dataframeFromMagpie.csv")


# Read the mapping CSV
mapping <- read.csv("prom-leap-mapping.csv",
                    stringsAsFactors = FALSE,
                    na.strings = c("NA", ""))

# Ensure the ID columns are integer
mapping <- mapping %>%
  mutate(across(c(branchId, variableId, scenarioId, regionId),
                ~ as.integer(.)))

# After you’ve built combinedMagpie and loaded mapping…
print("combinedMagpie columns:")
print(names(combinedMagpie))

print("mapping columns:")
print(names(mapping))

updatesDf <- combinedMagpie %>%
  inner_join(mapping,
             by = c("Scenario", "Region", "Variable")) %>%
  select(branchId, variableId, scenarioId, regionId, Year, Value)

updateLeapTemplateByIds(
  templatePath = "test.xlsx",
  outputPath = "leapImportFixedExample.xlsx",
  sheetName = "Export",
  updatesDf = updatesDf
)