#' calcMACC
#'
#' Construct OPENPROM input data from the IMAGE MACC dataset.
#'
#' This function reads the MACC data source, converts baseline emissions,
#' CH\eqn{_4} and N\eqn{_2}O marginal abatement cost (MAC) curves, and
#' F-gas MAC and baseline data into **MAgPIE** format.
#' It then performs temporal interpolation and spatial aggregation using the
#' IMAGE–OPENPROM mapping, producing a harmonized dataset suitable for use
#' as input to the OPENPROM module.
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

  list(x = finalMagpie,
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
