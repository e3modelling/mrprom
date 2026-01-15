#' readIEA_Energy_Projections_Balances2025
#'
#' Read in Energy_Projections_Balances from International Energy Agency.
#' The period is: 2030, 2040 and 2050 based on Business as usual.
#'
#' @param subtype Type of data that should be read.
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_Energy_Projections_Balances2025", subtype = "INDPROD")
#' }
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_wider_delim
#'
readIEA_Energy_Projections_Balances2025 <- function(subtype = "INDPROD") {
  
  if (!file.exists("Energy_Projections_Balances2025.rds")) {
    x <- read.csv("data_2020_2050_busasusual.csv", header = TRUE)
    x <- select(x, c("COUNTRY","ENERGY_BALANCE_FLOW","ENERGY_PRODUCT","TIME_PERIOD","OBS_VALUE","UNIT","SCENARIO"))
    names(x) <- c("region", "flow", "product", "period", "value", "unit","scenario")
    x[["region"]] <- factor(x[["region"]])
    x[["product"]] <- factor(x[["product"]])
    x[["flow"]] <- factor(x[["flow"]])
    x[["scenario"]] <- factor(x[["scenario"]])
    x[["unit"]] <- factor(x[["unit"]])
    x[["period"]] <- as.numeric(x[["period"]])
    x[["value"]] <- as.numeric(x[["value"]])
    
    saveRDS(object = x, file = "Energy_Projections_Balances2025.rds")
  }
  
  x <- readRDS("Energy_Projections_Balances2025.rds")
  
  if (subtype != "all") {
    x <- filter(x, x[["flow"]] == subtype)
  }
  x["unit"] <- "Mtoe"
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "Energy_Projections_Balances",
                       type = "Energy Projections Balances",
                       filename = "Energy_Projections_Balances.csv",
                       `Indicative size (MB)` = 46,
                       dimensions = "4D",
                       unit = "varius",
                       Confidential = "E3M"))
}
