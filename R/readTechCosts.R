#' readTechCosts
#'
#' Read in an excel file and convert it to a magpie object
#' The file containing the data is named TechCosts
#'
#' @param subtype A character string referring to the excel sheet, e.g. "PowerAndHeat" which would
#' read the excel sheet power_and_heat_energyf from the file TechCosts and convert it to a magpie object
#'
#' @return The read-in data into a magpie object
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' #' @examples
#' \dontrun{
#' a <- readSource("TechCosts")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#'

readTechCosts <- function(subtype = "PowerAndHeat") { # nolint

# categories and subcategories:
# demand / supply
# sectors: industry, transport, buildings, power&heat, etc.
# categories specific to each sector

.toolAddRef <- function(x, df) {
    return(c(0,
             unique(x[["variable"]]),
             unique(x[["efficiency_type"]]),
             names(df)[[1]],
             filter(df, df[, 1] == "Capital cost")[[1, 3]]))
}
.toolReadExcelWindow <- function(file, sheet, range) {
    x <- read_excel(path = file, sheet = sheet, range = range) # nolint
    x <- filter(x, x[[names(x)[2]]] != "NA") # remove empty rows
    x["variable"] <- names(x)[2] # store variable names in new column
    x["efficiency_type"] <- names(x)[1] # store efficiency type in new column
    names(x) <- c("efficiency_value",
                  as.character(x[1, grep("[a-z,A-Z]", as.character(x[1, ]), invert = TRUE)]),
                  names(x)[(length(x[1, ]) - 1) : length(x[1, ])])
    x <- filter(x, x[[1]] != "NA")
    x <- pivot_longer(x, cols = grep("[a-z,A-Z]", names(x), invert = TRUE), names_to = "period") # nolint
    return(x)
}
.toolReadExcelChunk <- function(range, rangeRef) {

      x <- .toolReadExcelWindow(file = file, sheet = sheet, range = range)
      df <- read_excel(path = file, sheet = sheet, range = rangeRef)
      if (grepl("Capital cost", df[, 1])) x <- rbind(.toolAddRef(x, df), x)
      return(x)
}


  if (subtype == "PowerAndHeat") {
    x <- read.csv("power_and_heat_energyf.csv")
    names(x) <- c("tech", "value", "year", "variable", "unit")
    x <- filter(x, x[["tech"]] != "Refurbishment of existing nuclear reactors")
    x[["value"]] <- as.numeric(x[["value"]])
    x <- as.quitte(x)
  } else if (subtype == "DomesticEnergy") {
      x <- read.csv("domestic_energyf2.csv")
      names(x) <- c("tech", "appliances", "sector", "value", "category", "measurement", "variant", "unit")
      x[["value"]] <- as.numeric(x[["value"]])
      x <- as.quitte(x)
  } else if (subtype == "IndustryEnergy") {
      x <- read.csv("industry_energyf2.csv")
      names(x) <- c("tech", "category", "value", "type", "measurement", "variant", "units")
      x[["value"]] <- as.numeric(x[["value"]])
      x <- as.quitte(x)
  } else if (subtype == "infrastructure") {
      x <- read.csv("Infrastructuref2.csv")
      names(x) <- c("tech", "value", "measurement","variable", "unit", "title")
      x[["value"]] <- as.numeric(x[["value"]])
      x <- as.quitte(x)
  } else if (subtype == "new_fuels_energy") {
      x <- read.csv("new_fuels_energy.csv")
      x <- as.quitte(x)
  } else if (subtype == "maritime") {
      file <- "REF2020_Technology Assumptions_Transport.xlsx"
      sheet <- "Maritime"

      x <- rbind(.toolReadExcelChunk("B10:F20", "B6:D8"), # diesel/fuel oil  Container 1000-1999 TEU vessel
                 .toolReadExcelChunk("B29:F39", "B25:D27"), # LNG Container 1000-1999 TEU vessel
                 .toolReadExcelChunk("B47:E57", "B44:D45"), # Fuel Cell  Container 1000-1999 TEU vessel
                 .toolReadExcelChunk("B65:E71", "B62:D63"), # Electric  Container 1000-1999 TEU vessel
                 .toolReadExcelChunk("B81:F91", "B77:D79"), # diesel/fuel oil  handymax-supra bulk carrier vessel
                 .toolReadExcelChunk("B100:F110", "B96:D98"), # LNG  handymax-supra bulk carrier vessel
                 .toolReadExcelChunk("B118:E128", "B115:D116"), # Fuel Cell handymax-supra bulk carrier vessel
                 .toolReadExcelChunk("B136:E142", "B133:D134"), # Electric handymax-supra bulk carrier vessel
                 .toolReadExcelChunk("B152:F162", "B148:D150"), # diesel/fuel oil General cargo vessel
                 .toolReadExcelChunk("B171:F181", "B167:D169"), # LNG General cargo vessel
                 .toolReadExcelChunk("B189:E199", "B186:D187"), # Fuel Cell General cargo vessel
                 .toolReadExcelChunk("B207:E213", "B204:D205"), # Electric General cargo vessel
                 .toolReadExcelChunk("B223:F233", "B219:D221"), # diesel/fuel oil  Tanker 10000-69999 DWT
                 .toolReadExcelChunk("B242:F252", "B238:D240"), # LNG  Tanker 10000-69999 DWT
                 .toolReadExcelChunk("B260:E270", "B257:D258"), # Fuel Cell  Tanker 10000-69999 DWT
                 .toolReadExcelChunk("B278:E284", "B275:D276") # Fuel Cell  Tanker 10000-69999 DWT
                 )

      x[["value"]] <- as.numeric(x[["value"]])
      x[["unit"]] <- sub("^.*. in ", "", x[["variable"]])
      x[["efficiency_unit"]] <- sub("^.*. \\(", "", x[["efficiency_type"]])
      x[["efficiency_unit"]] <- sub("\\)$", "", x[["efficiency_unit"]])
      x <- as.quitte(x)

  }

return(suppressWarnings(as.magpie(x)))
}
