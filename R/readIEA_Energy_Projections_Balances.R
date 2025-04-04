#' readIEA_Energy_Projections_Balances
#'
#' Read in Energy_Projections_Balances from International Energy Agency.
#' The period is: 2030, 2040 and 2050 based on national scenario modelling.
#'
#' @param subtype Type of data that should be read.
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_Energy_Projections_Balances", subtype = "INDPROD")
#' }
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_wider_delim
#'
readIEA_Energy_Projections_Balances <- function(subtype = "INDPROD") {
  
  if (!file.exists("Energy_Projections_Balances.rds")) {
    x <- read.csv2("Energy_Projections_Balances.csv")
    x <- data.frame(x[-1, ])
    x <- separate_wider_delim(x, cols = "x..1...", delim = ",", names = c("COUNTRY","FLOW","PRODUCT", "SCENARIO", "TIME", "VALUE"))
    names(x) <- c("region", "flow", "product", "scenario", "period", "value")
    x[["region"]] <- factor(x[["region"]])
    x[["product"]] <- factor(x[["product"]])
    x[["flow"]] <- factor(x[["flow"]])
    x[["scenario"]] <- factor(x[["scenario"]])
    x[["period"]] <- as.numeric(x[["period"]])
    x[["value"]] <- as.numeric(x[["value"]])
    
    saveRDS(object = x, file = "Energy_Projections_Balances.rds")
  }
  
  x <- readRDS("Energy_Projections_Balances.rds")
  
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
