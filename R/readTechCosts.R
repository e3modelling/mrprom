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
             df[[grep("Capital cost", df[, 1]), 3]]))
}
.toolReadExcelWindow <- function(file, sheet, range) {
    x <- read_excel(path = file, sheet = sheet, range = range) #nolint
    x <- filter(x, x[[names(x)[2]]] != "NA") # remove empty rows
    x["variable"] <- names(x)[2] # store variable names in new column
    x["efficiency_type"] <- names(x)[1] # store efficiency type in new column
    names(x) <- c("efficiency_value",
                  as.character(x[1, grep("[a-z,A-Z]", as.character(x[1, ]), invert = TRUE)]),
                  names(x)[(length(x[1, ]) - 1) : length(x[1, ])])
    x <- filter(x, x[[1]] != "NA")
    x <- pivot_longer(x, cols = grep("[a-z,A-Z]", names(x), invert = TRUE), names_to = "period") #nolint
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

      range <- "B10:F20"
      x <- .toolReadExcelWindow(file = file, sheet = sheet, range = range)
      range <- "B6:D8"
      df <- read_excel(path = file, sheet = sheet, range = range)
      if (grepl("Capital cost", df[, 1])) x <- rbind(.toolAddRef(x, df), x)

      range <- "B29:F39"
      tmp <- .toolReadExcelWindow(file = file, sheet = sheet, range = range)
      x <- rbind(x, tmp)
      range <- "B25:D27"
      df <- read_excel(path = file, sheet = sheet, range = range)
      if (grepl("Capital cost", df[, 1])) x <- rbind(.toolAddRef(tmp, df), x)


      x[["value"]] <- as.numeric(x[["value"]])
      x[["unit"]] <- sub("^.*. in ", "", x[["variable"]])
      x[["efficiency_unit"]] <- sub("^.*. \\(", "", x[["efficiency_type"]])
      x[["efficiency_unit"]] <- sub("\\)$", "", x[["efficiency_unit"]])
      x <- as.quitte(x)

  }

return(as.magpie(x))
}
