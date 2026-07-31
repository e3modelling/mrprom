#' readMAgPIE_HistoricalEmissions
#'
#' Read the frozen native-MAgPIE AFOLU history anchors from the madrat source
#' directory \code{MAgPIE_HistoricalEmissions}. The source contains the 39
#' OPEN-PROM regions at 2010, 2015, 2020 and 2025 for land-use-change CO2
#' emissions/removals (excluding indirect land CO2 and fire emissions),
#' Agriculture CH4 and Agriculture N2O. It is shared by the MAgPIE and GLOBIOM
#' land-use emulators.
#'
#' @return list with \code{x}, a magclass object with dimensions
#'   [region, year, emtype], and madrat metadata.
#' @author Songmin
#' @examples
#' \dontrun{
#' x <- readSource("MAgPIE_HistoricalEmissions", convert = FALSE)
#' }
#' @export
readMAgPIE_HistoricalEmissions <- function() {
  filename <- "MAgPIE_HistoricalEmissions.csv"
  if (!file.exists(filename)) {
    stop(
      "readMAgPIE_HistoricalEmissions: missing source file '",
      filename, "'"
    )
  }

  expectedMd5 <- "79af07fb463de016c277e5673c12ed0f"
  actualMd5 <- unname(tools::md5sum(filename))
  if (!identical(actualMd5, expectedMd5)) {
    stop(
      "readMAgPIE_HistoricalEmissions: source checksum mismatch; expected ",
      expectedMd5, ", found ", actualMd5
    )
  }

  data <- utils::read.csv(
    filename,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  idCols <- c("region", "emtype", "unit")
  anchorCols <- as.character(c(2010, 2015, 2020, 2025))
  missingCols <- setdiff(c(idCols, anchorCols), names(data))
  if (length(missingCols)) {
    stop(
      "readMAgPIE_HistoricalEmissions: missing required columns: ",
      paste(missingCols, collapse = ", ")
    )
  }
  if (!identical(names(data), c(idCols, anchorCols))) {
    stop("readMAgPIE_HistoricalEmissions: unexpected source columns")
  }

  expectedEmtypes <- c("CO2LandUse", "CH4LandUse", "N2OLandUse")
  expectedUnits <- c(
    CO2LandUse = "Mt CO2/yr",
    CH4LandUse = "Mt CH4/yr",
    N2OLandUse = "kt N2O/yr"
  )
  expectedH12 <- c(
    "CAZ", "CHA", "IND", "JPN", "LAM", "MEA",
    "NEU", "OAS", "REF", "SSA", "USA"
  )
  expectedEU28 <- c(
    "AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK",
    "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HRV",
    "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT",
    "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE"
  )
  expectedRegions <- c(expectedH12, expectedEU28)

  if (!setequal(unique(data$region), expectedRegions) ||
      !setequal(unique(data$emtype), expectedEmtypes)) {
    stop(
      "readMAgPIE_HistoricalEmissions: source must contain exactly OP39 and ",
      "CO2LandUse/CH4LandUse/N2OLandUse"
    )
  }
  key <- paste(data$region, data$emtype, sep = "\r")
  expectedKey <- as.vector(outer(expectedRegions, expectedEmtypes, paste, sep = "\r"))
  if (anyDuplicated(key) || !setequal(key, expectedKey)) {
    stop(
      "readMAgPIE_HistoricalEmissions: incomplete or duplicate ",
      "region/emtype grid"
    )
  }
  for (emtype in expectedEmtypes) {
    units <- unique(data$unit[data$emtype == emtype])
    if (length(units) != 1L || units != expectedUnits[[emtype]]) {
      stop(
        "readMAgPIE_HistoricalEmissions: unexpected unit for ", emtype,
        ": ", paste(units, collapse = ", ")
      )
    }
  }

  values <- as.matrix(data[anchorCols])
  storage.mode(values) <- "double"
  if (any(!is.finite(values))) {
    stop("readMAgPIE_HistoricalEmissions: source contains non-finite anchors")
  }

  long <- tidyr::pivot_longer(
    data[c("region", "emtype", anchorCols)],
    cols = dplyr::all_of(anchorCols),
    names_to = "period",
    values_to = "value"
  )
  long$period <- as.integer(long$period)
  long$value <- as.numeric(long$value)
  x <- magclass::as.magpie(
    as.data.frame(long[c("region", "period", "emtype", "value")]),
    spatial = 1,
    temporal = 2,
    datacol = 4
  )
  magclass::getSets(x) <- c("region", "year", "emtype")

  list(
    x = x,
    weight = NULL,
    unit = "CO2/CH4 Mt per yr; N2O kt per yr",
    description = c(
      category = "Land-use emulator common history",
      type = "Native MAgPIE AFOLU history anchors at OP39 resolution",
      filename = filename,
      dimensions = "OP39 region x 2010/2015/2020/2025 x emission type",
      Confidentiality = "open",
      comment = paste(
        "Native_SSP2_NPi_History_2026-07-29_16.12.19;",
        paste(
          "Land-use-change CO2 emissions/removals exclude indirect land CO2",
          "and fire emissions"
        )
      )
    )
  )
}
