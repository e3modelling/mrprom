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
  findOptimalPoints <- TRUE
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

  # Since the two datasets have costs, e.g., 3000 $ and indices like 15, we have to standardize names first.
  allNames <- getNames(finalMagpie)
  newNames <- allNames 

  # Regex to split "SF6_MAC" from "2"
  regexPattern <- "^(.*_MAC)_(\\d+)$"
  matches <- regexec(regexPattern, allNames)
  parsed  <- regmatches(allNames, matches)
  validIndices <- which(sapply(parsed, length) == 3)

  # Extract components
  stems   <- sapply(parsed[validIndices], `[`, 2)
  suffixes <- as.numeric(sapply(parsed[validIndices], `[`, 3))
  indices  <- validIndices

  # Detect "Index Sectors" vs "Cost Sectors"
  uniqueStems <- unique(stems)

  for(stem in uniqueStems) {
    # Find all variables for this sector
    mask <- (stems == stem)
    sectorSuffixes <- suffixes[mask]
    sectorIndices  <- indices[mask] # Where they are in the main list
    
    # LOGIC: If the maximum number is 201 or less, it's an INDEX.
    # If it goes up to 4000, it's already a COST.
    if (max(sectorSuffixes) <= 201) {
      
      # Calculate Real Cost: (Index - 1) * 20
      # Index 1 -> Cost 0
      # Index 2 -> Cost 20
      realCosts <- (sectorSuffixes - 1) * 20
      
      # Create new names
      currentNames <- newNames[sectorIndices]
      # Replace the suffix number with the calculated cost
      # We use sub() to replace the digits at the end
      updatedNames <- paste0(stem, "_", realCosts)
      
      # Update the master list
      newNames[sectorIndices] <- updatedNames
      
      #print(paste("   -> Converted", stem, ": Indices 1-201 mapped to Costs 0-4000"))
    }
  }

  getNames(finalMagpie) <- newNames

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

  # Apply the reduction
  patt <- paste0("_", optimalCosts, "$")
  finalVars <- grep(paste(patt, collapse="|"), getNames(finalMagpie), value=TRUE)

  # Add back the baseline emissions
  nonMac <- grep("_MAC_", getNames(finalMagpie), invert=TRUE, value=TRUE)
  finalVars <- unique(c(nonMac, finalVars))

  finalMagpieReduced <- finalMagpie[, , finalVars]

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
  costSteps <- seq(0, 4000, 20)
  
  # Capture Stem and Cost (Now we trust the suffix is ALWAYS the cost)
  regexPattern <- "^(.*_MAC)_(\\d+)$"
  matches <- regexec(regexPattern, allNames)
  parsed  <- regmatches(allNames, matches)
  validIndices <- which(sapply(parsed, length) == 3)
  
  stems   <- sapply(parsed[validIndices], `[`, 2)
  costs   <- as.numeric(sapply(parsed[validIndices], `[`, 3)) # IT IS NOW THE COST
  uniqueStems <- unique(stems)
  
  curveMatrix <- matrix(0, nrow = length(uniqueStems), ncol = length(costSteps))
  colnames(curveMatrix) <- costSteps
  dataFlat <- as.numeric(dimSums(data, dim = c(1, 2)))
  
  for (i in seq_along(uniqueStems)) {
    stem <- uniqueStems[i]
    mask <- (stems == stem)
    
    # Direct mapping: The suffix 20 IS column 20
    # We match column names to the suffix values
    theseCosts <- as.character(costs[mask])
    theseValues <- dataFlat[validIndices[mask]]
    
    # Fill matrix (column matching)
    curveMatrix[i, theseCosts] <- theseValues
  }
  
  # Normalize
  for(r in 1:nrow(curveMatrix)) {
    m <- max(curveMatrix[r,], na.rm=TRUE)
    if(m>0) curveMatrix[r,] <- curveMatrix[r,] / m
  }
  return(curveMatrix)
}
# Function to find the optimal cost points satisfy ALL sectors simultaneously
optimizeConservative <- function(curveMatrix, costSteps, threshold = 0.01) {
  keepIndices <- c(1, length(costSteps))
  xAxis <- costSteps
  repeat {
    keepIndices <- sort(unique(keepIndices))
    currX <- xAxis[keepIndices]
    reconstructed <- t(apply(curveMatrix[, keepIndices, drop=FALSE], 1, function(y) approx(currX, y, xout=xAxis, rule=2)$y))
    errors <- abs(curveMatrix - reconstructed)
    maxErr <- max(apply(errors, 2, max))
    if (maxErr < threshold) break
    keepIndices <- c(keepIndices, which.max(apply(errors, 2, max)))
  }
  return(xAxis[keepIndices])
}
