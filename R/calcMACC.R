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

  keepPattern <- paste0("_", optimalCosts, "$")
  regexPattern <- paste(keepPattern, collapse="|")
  allVars <- getNames(finalMagpie)

  # Keep variables matching the cost pattern
  keepVars <- grep(regexPattern, allVars, value = TRUE)

  # Also keep non-MAC variables (Baseline emissions, etc.) if they exist
  # Assumption: Non-MAC vars do not end in "_Number". HFC-43_10 is missed in the regex.
  nonMacVars <- grep("_\\d+$", allVars, invert = TRUE, value = TRUE)
  finalVars <- unique(c(keepVars, nonMacVars,'HFC-43_10'))
  finalMagpieReduced <- finalMagpie[, , finalVars]

  # Final renames before exporting
  rename_map <- c(
    "CH4_ent fermentation" = "CH4_enteric",
    "N2O_adip acid"           = "N2O_adipic",
    "N2O_nitr acid"          = "N2O_nitric",
    "CH4 from coal"          = "CH4_coal",
    "CH4 from oil"          = "CH4_oilp",
    "CH4 from gas"          = "CH4_ngas",
    "CH4 from domestic sewage"          = "CH4_sewage",
    "CH4 from landfills"          = "CH4_landfills",
    "CH4 from wetland rice production"          = "CH4_rice",
    "CH4 from animals / enteric fermentation"          = "CH4_enteric",
    "CH4 from animal waste"          = "CH4_manure",
    "N2O from transport"          = "N2O_transport",
    "N2O from adipic acid production"          = "N2O_adipic",
    "N2O from nitric acid production"          = "N2O_nitric",
    "N2O from fertilizer use"          = "N2O_fertilizer",
    "N2O from animal waste"          = "N2O_manure",
    "N2O Domestic sewage"          = "N2O_sewage",
    "HFC-"          = "HFC_"
  )
  current_names <- getNames(finalMagpieReduced)
  
  # Apply replacements
  # Using fixed=TRUE prevents Regex errors and usually matches faster
  for (old_term in names(rename_map)) {
    new_term <- rename_map[[old_term]]
    current_names <- gsub(old_term, new_term, current_names, fixed = TRUE)
  }

  getNames(finalMagpieReduced) <- current_names

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
      variable = paste0(variableName, "_", macLevel)
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

  # Detect Header
  headerRow <- which(df[[1]] == "t")[1]
  if (is.na(headerRow)) stop("Cannot detect header row for sheet: ", sheetName)
  colNames <- as.character(unlist(df[headerRow, ]))
  colNames[1] <- "year"
  colnames(df) <- colNames
  df <- df[(headerRow + 1):nrow(df), ]

  # --- A. BASELINE ---
  if (isBaseline) {
    names(df)[2] <- "region"
    longDf <- df %>%
      mutate(year = as.numeric(year), region = as.character(region)) %>%
      pivot_longer(cols = -c(year, region), names_to = "variable", values_to = "value") %>%
      mutate(value = as.numeric(value), year = as.character(year))
    return(as.magpie(longDf, spatial = "region", temporal = "year"))
  }

  # --- B. ABSOLUTE MAC (HFC + PFC) ---
  if (isHfcMacAbs || isPfcMacAbs) {
    df <- df[, 1:(ncol(df) - 2)]
    names(df)[2:3] <- c("gasClass", "carbonPrice")

    # DEFINE MAPPINGS
    hfcMap <- c("1" = "HFC_23", "2" = "HFC_32", "3" = "HFC_43_10", "4" = "HFC_125",
                "5" = "HFC_134a", "6" = "HFC_143a", "7" = "HFC_152a", "8" = "HFC_227ea",
                "9" = "HFC_236fa", "10" = "HFC_245ca")
    
    pfcMap <- c("1" = "CF4", "2" = "C2F6", "3" =  "C6F14")

    longDf <- df %>%
      pivot_longer(cols = -(year:carbonPrice), names_to = "region", values_to = "value") %>%
      mutate(
        year = as.character(as.numeric(year)),
        region = as.character(region),
        gasClass = as.character(gasClass),
        priceInput = as.numeric(carbonPrice),
        value = as.numeric(value)
      ) %>%
      mutate(
        # Map Class Number to Gas Name
        gasName = if (isHfcMacAbs) hfcMap[gasClass] else pfcMap[gasClass],
        # Fix Index vs Cost
        # If price is small (<=201), it's an Index -> Convert to Cost
        realCost = ifelse(priceInput <= 201, (priceInput - 1) * 20, priceInput),
        # Create Final Name: "Gas_Cost"
        variable = paste0(gasName, "_", realCost)
      ) %>%
      select(region, year, variable, value)

    return(as.magpie(longDf, spatial = "region", temporal = "year"))
  }

  # --- C. RELATIVE MAC (SF6) ---
  if (isSf6MacRel) {
    df <- df[, 1:(ncol(df) - 1)]
    names(df)[2] <- "carbonPrice"

    longDf <- df %>%
      pivot_longer(cols = -(year:carbonPrice), names_to = "region", values_to = "value") %>%
      mutate(
        year = as.character(as.numeric(year)),
        region = as.character(region),
        priceInput = as.numeric(carbonPrice),
        value = as.numeric(value)
      ) %>%
      mutate(
        # Fix Index vs Cost (SF6 usually has 1-201 indices)
        realCost = ifelse(priceInput <= 201, (priceInput - 1) * 20, priceInput),
        # Create Final Name: "SF6_Cost"
        variable = paste0("SF6_", realCost)
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
  
  # REGEX: Look for "Anything_Number"
  regexPattern <- "^(.*)_(\\d+)$"
  parsed <- str_match(allNames, regexPattern)
  
  validIdx <- which(!is.na(parsed[,1]))
  
  if(length(validIdx) == 0) stop("No Cost variables found (Format: Sector_Cost)")

  stems   <- parsed[validIdx, 2]
  costs   <- as.numeric(parsed[validIdx, 3])
  
  uniqueStems <- unique(stems)
  curveMatrix <- matrix(0, nrow = length(uniqueStems), ncol = length(costSteps))
  colnames(curveMatrix) <- as.character(costSteps) # Ensure character matching
  
  # Flatten Data
  dataFlat <- as.numeric(dimSums(data, dim = c(1, 2)))
  
  for (i in seq_along(uniqueStems)) {
    stem <- uniqueStems[i]
    mask <- (stems == stem)
    
    # 1. Extract potential costs and values for this sector
    theseCosts <- as.character(costs[mask])
    theseValues <- dataFlat[validIdx[mask]]
    
    # 2. CRITICAL FIX: Filter out costs that don't fit in our 0-4000 grid
    # This drops "4380" or "SF6_MAC_220" if it was miscalculated
    colsToFill <- intersect(theseCosts, colnames(curveMatrix))
    
    # 3. Only assign if we have matches
    if(length(colsToFill) > 0) {
      # We must match the values to the kept columns
      # We use match() to ensure values align with the filtered costs
      matchIdx <- match(colsToFill, theseCosts)
      
      curveMatrix[i, colsToFill] <- theseValues[matchIdx]
    }
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
