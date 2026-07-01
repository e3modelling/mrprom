#' readIEA2025
#'
#' The World Energy Balances data contains energy balances for 156 countries and
#' expressed in kilo tonnes of oil equivalent (ktoe). Conversion factors used
#' to calculate energy balances and indicators (including GDP, population,
#' industrial production index and ratios calculated with the energy data) are
#' also provided. The database also includes transparent notes on methodologies
#' and sources for country-level data. In general, the data are available from
#' 1971 (1960 for OECD countries) to 2024. Preliminary 2024 data are available
#' for select countries, products, and flows.
#'
#' @param subset flow : Type of data that should be read, e.g.
#' INDPROD :  production of primary energy or TOTENGY : Energy industry own use
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA2026", subtype = "INDPROD", convert = TRUE)
#' }
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_wider_delim
#'
readIEA2026 <- function(subset = "INDPROD") {
  if (!file.exists("Extended_energy_balances_01_07_2026.rds")) {
    x1 <- read.csv("IEAFAMBIG_1995_2014.csv", header = FALSE)
    x1 <- x1[-1,]
    x2 <- read.csv("IEAFAMBIG_2015_2024.csv", header = FALSE)
    x2 <- x2[-1,]
    
    x <- rbind(x1, x2)
    
    x <- x[,c("V3","V5","V7","V11","V15","V17")]
    
    names(x) <- c("region", "flow", "product", "period","value", "unit")
    
    
    fStartHorizon <- toolReadEvalGlobal(
      system.file(file.path("extdata", "main.gms"), package = "mrprom")
    )["fStartHorizon"]
    
    x <- x %>%
      filter(
        period >= fStartHorizon,
        !is.na(region)
      )
    
    saveRDS(object = x, file = "Extended_energy_balances_01_07_2026.rds")
  }
  
  setwd("C:/Users/sioutas/Ricardo Plc/Global Integrated Assessment Models - Documents/Work/PROMETHEUS Model/madratverse/sources/IEA2026")
  
  x <- readRDS("Extended_energy_balances_01_07_2026.rds") %>%
    mutate(value = as.numeric(value)) %>%
    drop_na(value) %>%
    filter(
      flow %in% subset
    ) %>%
    as.quitte() %>%
    as.magpie()
  
  list(
    x = x,
    weight = NULL,
    description = c(
      category = "Energy balances",
      type = "Energy balances until period 2023",
      filename = "EARLYBIG1.txt",
      `Indicative size (MB)` = 4000,
      dimensions = "3D",
      unit = "various",
      Confidential = "E3M"
    )
  )
}
