#' readIEA2024
#'
#' The World Energy Balances data contains energy balances for 156 countries and
#' expressed in kilo tonnes of oil equivalent (ktoe). Conversion factors used
#' to calculate energy balances and indicators (including GDP, population, 
#' industrial production index and ratios calculated with the energy data) are
#' also provided. The database also includes transparent notes on methodologies
#' and sources for country-level data. In general, the data are available from
#' 1971 (1960 for OECD countries) to 2022. Preliminary 2023 data are available
#' for select countries, products, and flows.
#'
#' @param subtype flow : Type of data that should be read, e.g. 
#' INDPROD :  production of primary energy or TOTENGY : Energy industry own use
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA2024", subtype = "INDPROD", convert = TRUE)
#' }
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_wider_delim
#'
readIEA2024 <- function(subtype = "INDPROD") {
  
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
  
  x <- filter(x, !is.na(x[["region"]]))
  if (subtype != "all") {
    x <- filter(x, x[["flow"]] == subtype)
  }
  x["unit"] <- "ktoe"
  x <- as.quitte(x)
  x <- as.magpie(x)
  return(x)
}
