#' calcEnergyCosts
#'
#' Read EnergyCosts data from the "EU Reference Scenario":
#' https://energy.ec.europa.eu/data-and-analysis/energy-modelling/eu-reference-scenario-2020_en
#' and convert it to a csv file
#'
#' @param subtype Type of data that should be read. The type is referring to the
#' excel sheet, from the excel file "REF2020_Technology Assumptions_Energy.xlsx"
#' Available types are:
#' \itemize{
#' \item `PowerAndHeat`:
#' \item `DomesticEnergy`:
#' \item `IndustryEnergy`:
#' \item `new_fuels_energy`:
#' \item `renovation_costs`:
#' }
#'
#' @return Read EnergyCosts data and convert it to a csv file
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "EnergyCosts", file = "PowerAndHeat.csv", aggregate = FALSE)
#' }
#' @importFrom quitte as.quitte


calcEnergyCosts <- function(subtype = "PowerAndHeat") {
  if (subtype == "PowerAndHeat") {
    x <- readSource("TechCosts", subtype)
    u <- getItems(x, "unit")
    x <- as.quitte(x)
    x <- x[, -c(5)]

    } else if (subtype == "DomesticEnergy") {
      x <- readSource("TechCosts", subtype)
      u <- getItems(x, "unit")
      x <- as.quitte(x)
      x[["variable"]] <- x[["category"]]
      x <- x[x[["levels"]] == "medium", ]
      x <- x[, -c(5, 9:11)]

    } else if (subtype == "IndustryEnergy") {
      x <- readSource("TechCosts", subtype)
      u <- "1"
      x <- as.quitte(x)
      x[["variable"]] <- x[["category_of_technology"]]
      x <- x[x[["levels"]] == "medium", ]
      x <- x[, -c(5, 9, 10)]

    } else if (subtype == "new_fuels_energy") {
      x <- readSource("TechCosts", subtype)
      u <- "1"
      x <- as.quitte(x)
      x <- x[, -c(5, 9)]
      colnames(x)[7] <- "technology"

    }  else if (subtype == "renovation_costs") {
      x <- readSource("TechCosts", subtype)
      u <- getItems(x, "unit")
      x <- as.quitte(x)
      x[["variable"]] <- x[["type.of.renovation.measure..building.envelope.refurbishment."]]
      x <- x[, -c(9, 10)]
      colnames(x)[5] <- "technology"

    }

  x[["value"]] <- as.numeric(x[["value"]])
  x[["variable"]] <- as.factor(x[["variable"]])
  x[["period"]] <- as.factor(x[["period"]])

  x[["variable"]] <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",
                          x[["variable"]], perl = TRUE)
  x[["variable"]] <- gsub("[\r\n]", "_", x[["variable"]])
  x[["variable"]] <- gsub(" ", "_", x[["variable"]])
  x[["variable"]] <- gsub(",", "", x[["variable"]])

  x[["technology"]] <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "",
                            x[["technology"]], perl = TRUE)
  x[["technology"]] <- gsub("[\r\n]", "_", x[["technology"]])
  x[["technology"]] <- gsub(" ", "_", x[["technology"]])
  x[["technology"]] <- gsub(",", "", x[["technology"]])

  x <- as.quitte(x)
  x <- as.magpie(x)

  return(list(x = x,
              weight = NULL,
              unit = u,
              description = "readTechCosts; EnergyCosts; from the EU Reference Scenario"))
}
