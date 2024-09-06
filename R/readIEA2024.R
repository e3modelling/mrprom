#' readIEA2024
#'
#' Read in energy balances from International Energy Agency until period 2022.
#'
#' @param subtype Type of data that should be read.
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA2024", subtype = "MAINELEC")
#' }
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_wider_delim
#'
readIEA2024 <- function(subtype = "MAINELEC") {
  
  if (!file.exists("Extended_energy_balances_25_7_2024.rds")) {
    x <- read.csv2("Extended_energy_balances_25_7_2024.csv")
    x <- data.frame(x[-1, ])
    x <- separate_wider_delim(x, cols = "x..1...", delim = ",", names = c("COUNTRY","TIME","PRODUCT","FLOW", "VALUE"))
    names(x) <- c("region", "period", "product", "flow", "value")
    x[["region"]] <- factor(x[["region"]])
    x[["product"]] <- factor(x[["product"]])
    x[["flow"]] <- factor(x[["flow"]])
    x[["period"]] <- as.numeric(x[["period"]])
    x[["value"]] <- as.numeric(x[["value"]])
    saveRDS(object = x, file = "Extended_energy_balances_25_7_2024.rds")
  }
  
  x <- readRDS("Extended_energy_balances_25_7_2024.rds")
  
  x[["region"]] <- toolCountry2isocode(x[["region"]], mapping =
                                         c("Bolivarian Republic of Venezuela" = "VEN",
                                           "China (P.R. of China and Hong Kong, China)" = "CHA",
                                           "Kingdom of Eswatini" = "SWZ",
                                           "Republic of the Congo" = "COG",
                                           "Republic of Turkiye" = "TUR",
                                           "IEAFAMILY" = "GLO"))
  x <- filter(x, !is.na(x[["region"]]))
  if (subtype != "all") {
    x <- filter(x, x[["flow"]] == subtype)
  }
  x <- as.quitte(x)
  x["unit"] <- "ktoe"
  x <- as.magpie(x)
  x <- toolCountryFill(x)
  x <- collapseDim(x, dim = c(3.1, 3.2, 3.3))
  return(x)
}
