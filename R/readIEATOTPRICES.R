#' readIEATOTPRICES
#'
#' Read in energy prices from International Energy Agency per sector, period, country and fuel.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEATOTPRICES")
#' }
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_wider_delim separate
#'
readIEATOTPRICES <- function() {
  
  if (!file.exists("IEATOTPRICES.rds")) {
    
    x <- read.csv("OECD.IEA,ENDPRICES,1.1,filtered,2025-07-22 15-55-59.csv",header=TRUE)
    x <- select(x, "COUNTRY_SUBREG", "ENERGY_PRODUCT", "SECTOR", "CURRENCY",
                "Unit", "TIME_PERIOD", "OBS_VALUE")
    names(x) <- c("region", "fuel", "variable", "CURRENCY", "unit", "period", "value")
    x <- filter(x, CURRENCY == "USD_R")
    x[["region"]] <- factor(x[["region"]])
    x[["fuel"]] <- factor(x[["fuel"]])
    x[["variable"]] <- factor(x[["variable"]])
    x[["unit"]] <- factor(x[["unit"]])
    x[["period"]] <- as.numeric(x[["period"]])
    x[["value"]] <- as.numeric(x[["value"]])
    x[["CURRENCY"]] <- factor(x[["CURRENCY"]])
    x <- filter(x,!is.na(value))
    x <- as.quitte(x)
    
    saveRDS(object = x, file = "IEATOTPRICES.rds")
  }
  
  x <- readRDS("IEATOTPRICES.rds")
  
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "Costs",
                       type = "Energy prices",
                       filename = "OECD.IEA,ENDPRICES,1.1,filtered,2025-07-22 15-55-59.csv",
                       `Indicative size (MB)` = 20,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "E3M"))
}
