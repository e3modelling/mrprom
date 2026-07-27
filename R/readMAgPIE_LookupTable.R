#' readMAgPIE_LookupTable
#'
#' Read the vertically combined MAgPIE biomass and AFOLU lookup table and
#' return it as a magclass object. The source is already disaggregated to the
#' 39 OPEN-PROM regions; no regional mapping is applied in mrprom.
#'
#' The source file is
#' \code{MAgPIE_LookupTable/MAgPIE_LookupTable_contract.xlsx}. Subtype
#' \code{openprom} reads \code{lookup_table_openprom}; subtype \code{h12} reads
#' \code{lookup_table_h12}. Required identifier columns are \code{Region},
#' \code{Variable}, \code{Unit}, \code{Standard}, \code{BioDem}, and
#' \code{GHGPrice}; all four-digit columns are interpreted as years.
#'
#' @param subtype Regional resolution: \code{"openprom"} (OP39) or
#'   \code{"h12"} (the 12 MAgPIE coupling regions, plus an optional World row).
#' @return list with \code{x}, a magclass object with dimensions
#'   [region, year, variable.biodem.ghgscen], and madrat metadata.
#' @author Songmin
#' @examples
#' \dontrun{
#' x <- readSource("MAgPIE_LookupTable", convert = FALSE)
#' }
#' @seealso \code{\link{calcBmswasLandEmisCoefMAgPIE}},
#'   \code{\link{calcBmswasBioPriceH12MAgPIE}},
#'   \code{\link{calcBmswasAgriEmisCoefMAgPIE}}
#' @importFrom dplyr all_of
#' @importFrom magclass as.magpie getSets<-
#' @importFrom readxl excel_sheets read_excel
#' @importFrom tidyr pivot_longer
#' @export
readMAgPIE_LookupTable <- function(subtype = "openprom") {
  filename <- "MAgPIE_LookupTable_contract.xlsx"
  if (!file.exists(filename)) {
    stop("readMAgPIE_LookupTable: missing source file '", filename, "'")
  }
  subtype <- match.arg(tolower(subtype), c("openprom", "h12"))
  sheets <- excel_sheets(filename)
  sheet <- if (subtype == "openprom") {
    "lookup_table_openprom"
  } else {
    "lookup_table_h12"
  }
  if (!(sheet %in% sheets)) {
    stop("readMAgPIE_LookupTable: missing sheet '", sheet, "' in ", filename,
         " for subtype '", subtype, "'")
  }

  df <- read_excel(filename, sheet = sheet)
  idCols <- c("Region", "Variable", "Unit", "Standard", "BioDem", "GHGPrice")
  missingCols <- setdiff(idCols, names(df))
  if (length(missingCols)) {
    stop("readMAgPIE_LookupTable: missing required columns: ",
         paste(missingCols, collapse = ", "))
  }
  ycols <- grep("^[0-9]{4}$", names(df), value = TRUE)
  if (!length(ycols)) {
    stop("readMAgPIE_LookupTable: no four-digit year columns found")
  }

  for (k in idCols) {
    if (any(is.na(df[[k]]) | !nzchar(trimws(as.character(df[[k]]))))) {
      stop("readMAgPIE_LookupTable: missing identifier in column '", k, "'")
    }
    df[[k]] <- trimws(as.character(df[[k]]))
  }

  rowKey <- do.call(paste, c(df[c("Region", "Variable", "BioDem", "GHGPrice")],
                            sep = "\r"))
  if (anyDuplicated(rowKey)) {
    stop("readMAgPIE_LookupTable: duplicate Region/Variable/BioDem/GHGPrice rows")
  }

  requiredUnitsOpenprom <- c(
    "Primary Energy|Biomass|2nd Generation|Energy Crops" = "EJ/yr",
    "Primary Energy|Biomass|2nd Generation|Crop Residues" = "EJ/yr",
    "Primary Energy|Biomass|2nd Generation" = "EJ/yr",
    "Price|Primary Energy|Biomass|2nd Generation|Energy Crops" = "USD_2017/GJ",
    "Price|Primary Energy|Biomass|2nd Generation|Crop Residues" = "USD_2017/GJ",
    "Emissions|CO2|AFOLU|Land" = "Mt CO2/yr",
    "Emissions|CH4|AFOLU|Agriculture" = "Mt CH4/yr",
    "Emissions|N2O|AFOLU|Agriculture" = "kt N2O/yr"
  )
  requiredUnitsH12 <- c(
    "Primary Energy|Biomass|2nd Generation|Requested" = "EJ/yr",
    "Price|Primary Energy|Biomass|Native" = "USD_2017/GJ",
    "Emissions|CO2|AFOLU|Land" = "Mt CO2/yr",
    "Emissions|CH4|AFOLU|Agriculture" = "Mt CH4/yr",
    "Emissions|N2O|AFOLU|Agriculture" = "kt N2O/yr"
  )
  requiredUnits <- if (subtype == "h12") requiredUnitsH12 else requiredUnitsOpenprom
  missingVars <- setdiff(names(requiredUnits), unique(df$Variable))
  if (length(missingVars)) {
    stop("readMAgPIE_LookupTable: variables required by the emulator are missing: ",
         paste(missingVars, collapse = "; "))
  }
  for (v in names(requiredUnits)) {
    units <- unique(df$Unit[df$Variable == v])
    if (length(units) != 1L || units != requiredUnits[[v]]) {
      stop("readMAgPIE_LookupTable: unexpected unit for '", v, "': ",
           paste(units, collapse = ", "), " (expected ", requiredUnits[[v]], ")")
    }
  }

  long <- pivot_longer(
    df[c("Region", "Variable", "BioDem", "GHGPrice", ycols)],
    cols = all_of(ycols), names_to = "period", values_to = "value"
  )
  long$period <- as.integer(long$period)
  long$value <- suppressWarnings(as.numeric(long$value))

  x <- as.magpie(
    as.data.frame(long[c("Region", "period", "Variable", "BioDem", "GHGPrice", "value")]),
    spatial = 1, temporal = 2, datacol = 6
  )
  getSets(x) <- c("region", "year", "variable", "biodem", "ghgscen")

  list(
    x = x,
    weight = NULL,
    unit = "mixed (EJ/yr; USD_2017/GJ; Mt or kt/yr)",
    description = c(
      category = "Biomass",
      type = "MAgPIE second-generation biomass supply and AFOLU lookup",
      filename = filename,
      `Indicative size (MB)` = "10",
      dimensions = paste(subtype, "region x year x variable.biodem.ghgscen"),
      unit = "see Variable/Unit columns in source workbook",
      Confidentiality = "open",
      comment = paste(
        "Four MAgPIE carbon-policy runs, vertically combined;",
        "requested-Q/native-price contract fields are available at H12"
      )
    )
  )
}
