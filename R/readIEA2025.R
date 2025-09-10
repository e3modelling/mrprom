#' readIEA2025
#'
#' The World Energy Balances data contains energy balances for 156 countries and
#' expressed in kilo tonnes of oil equivalent (ktoe). Conversion factors used
#' to calculate energy balances and indicators (including GDP, population, 
#' industrial production index and ratios calculated with the energy data) are
#' also provided. The database also includes transparent notes on methodologies
#' and sources for country-level data. In general, the data are available from
#' 1971 (1960 for OECD countries) to 2023. Preliminary 2023 data are available
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
#' a <- readSource("IEA2025", subtype = "INDPROD", convert = TRUE)
#' }
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_wider_delim
#'
readIEA2025 <- function(subtype = "INDPROD") {
  
  if (!file.exists("Extended_energy_balances_11_7_2025.rds")) {
    
    x1 <- read.table("EARLYBIG1.txt", header = FALSE)
    x2 <- read.table("EARLYBIG2.txt", header = FALSE)
    
    x <- rbind(x1, x2)
    
    x <- x[,-7]
    
    names(x) <- c("region", "product", "period", "flow", "unit", "value")
    
    saveRDS(object = x, file = "Extended_energy_balances_11_7_2025.rds")
  }
  
  x <- readRDS("Extended_energy_balances_11_7_2025.rds")
  
  x <- filter(x, !is.na(x[["region"]]))
  if (subtype != "all") {
    x <- filter(x, x[["flow"]] == subtype)
  }

  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "Energy balances",
                       type = "Energy balances until period 2023",
                       filename = "EARLYBIG1.txt",
                       `Indicative size (MB)` = 4000,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "E3M"))
}
