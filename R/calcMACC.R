#' calcMACC
#'
#' Construct OPENPROM input data from the IMAGE MACC dataset.
#'
#' This function reads the IMAGE Marginal Abatement Cost Curve (MACC) data
#' and converts baseline emissions and marginal abatement cost curves into
#' **MAgPIE**-compatible magpie objects. It processes: baseline emissions for all gases,
#'  MAC curves for CH\eqn{_4} and N\eqn{_2}O, absolute MAC curves for HFCs and PFCs,
#'  relative MAC curves for SF\eqn{_6}.
#'
#' The function: 1) Reads the MACC data source using \code{readSource()}, 
#' converts baseline emissions and MAC curves to magpie format,
#' interpolates all time series to a common annual grid,
#' aggregates data from IMAGE regions to OPENPROM regions,
#' reduces MAC curve resolution using predefined or optimized carbon price points.
#' 
#' Helper functions are used to parse and normalize MAC curves and to
#' conservatively select representative cost points across all sectors.
#'
#' @return A magpie object having baseline emissions and marginal abatement costs
#' for CH4, N2O and F-gases
#'
#' @author Anastasis Giannousakis, Alexandros Tsimpoukis
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "MACC", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% select mutate rename
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom magclass as.magpie mbind getItems time_interpolate
#' @importFrom madrat readSource toolGetMapping toolAggregate
#' @importFrom quitte as.quitte


calcMACC <- function() {
  # Parameter to run optimization and reduce the final points
  findOptimalPoints <- FALSE
  # load data source
  x <- readSource("MACC", convert = FALSE)
  
  # use image-openprom mapping
  map <- toolGetMapping(name = "open-prom-image-mapping.csv",
                        type = "regional",
                        where = "mrprom")
  magpieList <- list()
  # Baseline emissions
  df <- as.data.frame(x[[1]][[1]])
  baselineEmissions <- convertEmissionsBaselineToMagpie(df)

  # Marginal abatement costs - MAC for CH4 and N2O
  SSP2Sheets <- x[[1]][2:length(x[[1]])]

  for (i in seq_along(SSP2Sheets)) {
    sheetName <- names(SSP2Sheets)[i]
    sheet <- SSP2Sheets[[i]]
    # Extract variable name (everything after "SSP2 ")
    variableName <- sub("^SSP2\\s+", "", sheetName)
    magpieList[[sheetName]] <- convertMacToMagpie(sheet, variableName)
  }
  CH4N20MAC <- do.call(mbind, magpieList)

  # Marginal abatement costs - MAC for F-gases
  magpieList <- list()
  fgasSheets <- x[[2]][3:length(x[[2]])]
  targetYears <- paste0("y", 2010:2100)
  for (sheetName in names(fgasSheets)) {
    df <- fgasSheets[[sheetName]]
    magpiedObj <- convertFgasesToMagpie(df, sheetName)
    magpieList[[sheetName]] <- time_interpolate(magpiedObj, targetYears)
  }
  Fgases <- do.call(mbind, magpieList)

  
  # Reduce the years to a smaller range, from 2010-2100.
  CH4N20MAC <- CH4N20MAC[, getItems(baselineEmissions,2),]
  Fgases <- Fgases[, getItems(baselineEmissions,2),]
  finalMagpie <- mbind(baselineEmissions, CH4N20MAC, Fgases)
  finalMagpie <- time_interpolate(finalMagpie, targetYears)

  finalMagpie <- toolAggregate(
  x = finalMagpie,
  rel = map,
  from = "IMAGE.Region",
  to = "ISO3.Code",
  weight = NULL
  )

  # Reduce data points, either use the already found optimal values or rerun optimization
  if (findOptimalPoints == TRUE) {
    # Build the matrix of all 100+ normalized curves
    normalizedCurves <- getDataMatrix(finalMagpie)

    # Run the optimizer (Threshold 0.01 = 1% Max Deviation allowed for ANY sector)
    fullCosts <- seq(0, 4000, 20)
    optimalCosts <- optimizeConservative(normalizedCurves, fullCosts, threshold = 0.01)
  
  } else {
    # Hardcoded optimal costs list
    optimalCosts <- c(
      0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 220, 240, 260, 280,
      300, 320, 340, 360, 380, 400, 420, 460, 480, 500, 520, 540, 560, 580, 600,
      660, 680, 720, 740, 760, 780, 820, 840, 1000, 1080, 1100, 1120, 1520, 1660, 1700,
      1740, 2580, 2600, 3440, 3460, 3500, 3540, 3600, 3840, 4000)
  }

  # APPLY THE REDUCTION
  # Standard Names (Suffix = Cost) Regex: _(0|20|100)$
  pattCost <- paste0("_", optimalCosts, "$")
  standardKeep <- grep(paste(pattCost, collapse="|"), getNames(finalMagpie), value=TRUE)

  # Indexed Names (Suffix = Index)
  # Convert Cost back to Index: Index = (Cost / 20) + 1
  optimalIndices <- (optimalCosts / 20) + 1

  # Look for "_MAC_" + Index + EndOfString
  regexIdx <- paste0("MAC_", optimalIndices, "$")
  indexedKeep <- grep(paste(regexIdx, collapse="|"), getNames(finalMagpie), value=TRUE)

  # Combine variables
  finalVars <- c(standardKeep, indexedKeep)
  # Add back any non-MAC variables (like "CH4 from coal") that don't have "_MAC_"
  nonMacVars <- grep("_MAC_", getNames(finalMagpie), invert=TRUE, value=TRUE)
  finalVars <- c(nonMacVars, finalVars)
  # Avoid duplicates
  finalVarsUnique <- unique(finalVars)
  finalMagpieReduced <- finalMagpie[, , finalVarsUnique]

  list(x = finalMagpieReduced,
       weight = NULL,
       unit = "various",
       description = "A MAgPIE object containing baseline emissions and all MAC curves aggregated to OPENPROM regions.")

}
# Helpers ------------------------------------------------------------------------------------
convertEmissionsBaselineToMagpie <- function(df) {
  longDf <- df %>%
    pivot_longer(
      cols = -c(Year, Region),
      names_to = "variable",
      values_to = "value"
    ) %>%
    rename(
      year = Year,
      region = Region
    ) %>%
    mutate(
      year = as.character(year),
      region = as.character(region)
    )

  magpieObj <- as.magpie(
    longDf,
    spatial = "region",
    temporal = "year",
  )

  return(magpieObj)
}
convertMacToMagpie <- function(df, variableName) {

  longDf <- df %>%
    pivot_longer(
      cols = -c(t, `Implementation costs`),
      names_to = "region",
      values_to = "value"
    ) %>%
    rename(
      year = t,
      macLevel = `Implementation costs`
    ) %>%
    mutate(
      year = as.character(year),
      region = as.character(region),
      variable = paste0(variableName, "_MAC_", macLevel)
    ) %>%
    select(region, year, variable, value)

  magpieObj <- as.magpie(
    longDf,
    spatial = "region",
    temporal = "year"
  )

  return(magpieObj)
}

convertFgasesToMagpie <- function(df, sheetName) {

  sheetLower <- tolower(sheetName)

  isBaseline <- grepl("baseline", sheetLower)
  isHfcMacAbs <- grepl("hfc", sheetLower) && grepl("cost", sheetLower)
  isPfcMacAbs <- grepl("pfc", sheetLower) && grepl("cost", sheetLower)
  isSf6MacRel <- grepl("sf6", sheetLower) && grepl("relative", sheetLower)

  # ---------------------------------------------------------
  # DETECT header row with REAL column names
  # ---------------------------------------------------------
  headerRow <- which(df[[1]] == "t")[1]

  if (is.na(headerRow)) {
    stop("Cannot detect header row for sheet: ", sheetName)
  }

  # Extract true column names
  colNames <- as.character(unlist(df[headerRow, ]))
  colNames[1] <- "year"
  colnames(df) <- colNames

  # Drop header rows
  df <- df[(headerRow + 1):nrow(df), ]

  # ---------------------------------------------------------
  # BASELINE EMISSIONS
  # ---------------------------------------------------------

  if (isBaseline) {

    names(df)[2] <- "region"

    longDf <- df %>%
      mutate(
        year = as.numeric(year),
        region = as.character(region)
      ) %>%
      pivot_longer(
        cols = -c(year, region),
        names_to = "variable",
        values_to = "value"
      ) %>%
      mutate(
        value = as.numeric(value),
        year = as.character(year),
        variable = make.names(variable)
      ) %>%
      select(region, year, variable, value)

    return(as.magpie(longDf, spatial = "region", temporal = "year"))
  }

  # ---------------------------------------------------------
  # ABSOLUTE MAC curves (HFC + PFC)
  # ---------------------------------------------------------

  if (isHfcMacAbs || isPfcMacAbs) {

    # Drop the last 2 columns to avoid come irreleavant columns
    df <- df[, 1:(ncol(df) - 2)]

    names(df)[2:3] <- c("gasClass", "carbonPrice")

    df <- df %>%
      mutate(
        year = as.numeric(year),
        gasClass = as.character(gasClass),
        carbonPrice = as.numeric(carbonPrice)
      )

    longDf <- df %>%
      pivot_longer(
        cols = -(year:carbonPrice),
        names_to = "region",
        values_to = "value"
      ) %>%
      mutate(
        value = as.numeric(value),
        year = as.character(year),
        region = as.character(region),
        variable = paste0(
          ifelse(isHfcMacAbs, "HFC", "PFC"),
          "_class_", gasClass,
          "_MAC_", carbonPrice
        )
      ) %>%
      select(region, year, variable, value)

    return(as.magpie(longDf, spatial = "region", temporal = "year"))
  }

  # ---------------------------------------------------------
  # RELATIVE MAC (SF6)
  # ---------------------------------------------------------

  if (isSf6MacRel) {

    # Drop the last column
    df <- df[, 1:(ncol(df) - 1)]

    names(df)[2] <- "carbonPrice"

    df <- df %>%
      mutate(
        year = as.numeric(year),
        carbonPrice = as.numeric(carbonPrice)
      )

    longDf <- df %>%
      pivot_longer(
        cols = -(year:carbonPrice),
        names_to = "region",
        values_to = "value"
      ) %>%
      mutate(
        value = as.numeric(value),
        year = as.character(year),
        region = as.character(region),
        variable = paste0("SF6_MAC_", carbonPrice)
      ) %>%
      select(region, year, variable, value)

    return(as.magpie(longDf, spatial = "region", temporal = "year"))
  }

  stop("Unknown F-gas sheet type: ", sheetName)
}
# Function to parse all variable names and map them to the cost points
getDataMatrix <- function(data) {
  
  allNames <- getNames(data)
  
  # Standard Costs (0, 20... 4000) -> Expected: 201 steps
  costSteps <- seq(0, 4000, 20)
  
  # Prepare a container: Rows = Unique Curves (e.g. CH4_Coal), Cols = 201 Costs
  # We identify unique "Stems" by removing the suffix
  # For CH4_coal_MAC_20 -> Stem: CH4_coal_MAC
  # For SF6_MAC_1       -> Stem: SF6_MAC
  
  # Regex to capture the Stem and the Suffix
  # Matches "Everything_MAC_" then "Digits" at the end of the string
  regexPattern <- "^(.*_MAC)_(\\d+)$"
  
  matches <- regexec(regexPattern, allNames)
  parsed  <- regmatches(allNames, matches)
  
  # Filter out items that didn't match the MAC pattern (like "CH4 from coal")
  validIndices <- which(sapply(parsed, length) == 3)
  
  if(length(validIndices) == 0) stop("No MAC variables found matching pattern *_MAC_#")
  
  # Create a lookup table
  stems   <- sapply(parsed[validIndices], `[`, 2)
  suffix  <- as.numeric(sapply(parsed[validIndices], `[`, 3))
  originalIdx <- validIndices
  
  uniqueStems <- unique(stems)
  nStems <- length(uniqueStems)
  nCosts <- length(costSteps)
  
  # Create the matrix to hold all normalized curves
  # Dimensions: [Curves x Costs]
  curveMatrix <- matrix(NA, nrow = nStems, ncol = nCosts)
  rownames(curveMatrix) <- uniqueStems
  colnames(curveMatrix) <- costSteps
  
  cat(paste("Processing", nStems, "unique MAC curves across", nCosts, "cost steps...\n"))
  
  # Sum data over Region and Year to get the "Global Time-Averaged Shape" for each curve
  # dimSums is fast.
  dataFlat <- dimSums(data, dim = c(1, 2)) 
  
  for (i in 1:nStems) {
    stem <- uniqueStems[i]
    
    # Find all variables belonging to this stem
    idxMask <- (stems == stem)
    currentSuffixes <- suffix[idxMask]
    currentDataIndices <- originalIdx[idxMask]
    
    # Determine if suffix is "Cost" (0, 20...) or "Index" (1, 2...)
    colIndices <- numeric(length(currentSuffixes))
    
    if (max(currentSuffixes) > 201) {
      # It is Explicit Cost (e.g. 0, 20, 4000)
      # Map 0->1, 20->2, 40->3 ...
      colIndices <- (currentSuffixes / 20) + 1
    } else {
      # It is an Index (e.g. 1, 2... 201)
      # Map 1->1, 2->2 ...
      colIndices <- currentSuffixes
    }
    
    # Extract values
    values <- as.numeric(dataFlat[currentDataIndices])
    
    # Place into matrix
    # If duplicates exist (rare), we sum/mean them. Assuming 1:1 mapping.
    curveMatrix[i, colIndices] <- values
  }
  
  # --- NORMALIZE CURVES (0 to 1) ---
  # This makes "Small Sectors" just as important as "Big Sectors" for shape detection
  for(r in 1:nrow(curveMatrix)) {
    rowMax <- max(curveMatrix[r, ], na.rm = TRUE)
    if(rowMax > 0) {
      curveMatrix[r, ] <- curveMatrix[r, ] / rowMax
    } else {
      curveMatrix[r, ] <- 0 # Flat zero curve
    }
  }
  
  # Fill NAs with 0
  curveMatrix[is.na(curveMatrix)] <- 0
  
  return(curveMatrix)
}
# Function to find the optimal cost points satisfy ALL sectors simultaneously
optimizeConservative <- function(curveMatrix, costSteps, threshold = 0.01) {
  
  keepIndices <- c(1, length(costSteps)) 
  xAxis <- costSteps
  
  repeat {
    keepIndices <- sort(unique(keepIndices))
    
    # Current grid
    currX <- xAxis[keepIndices]
    
    # Interpolate the *kept* columns to reconstruct the full matrix
    # We use apply to run approx() on every row
    reconstructedMatrix <- t(apply(curveMatrix[, keepIndices, drop=FALSE], 1, function(ySub) {
      approx(currX, ySub, xout = xAxis, rule=2)$y
    }))
    
    # Calculate Abs Error Matrix
    errorMatrix <- abs(curveMatrix - reconstructedMatrix)
    
    # Find the maximum error occurring at any single cost point across ALL sectors
    maxErrorProfile <- apply(errorMatrix, 2, max)
    
    globalMaxError <- max(maxErrorProfile)
    if (globalMaxError < threshold) break
    
    # Find the candidate point that has the highest error
    worstIdx <- which.max(maxErrorProfile)
    
    if (worstIdx %in% keepIndices) break 
    keepIndices <- c(keepIndices, worstIdx)
    
    if (length(keepIndices) == length(xAxis)) break
  }
  
  return(xAxis[keepIndices])
}
