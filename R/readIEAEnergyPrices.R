#' readIEAEnergyPrices
#'
#' Read in energy prices from International Energy Agency per sector, period, country and fuel.
#'
#' @param subtype Type of data that should be read.
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEAEnergyPrices", subtype = "RESID")
#' }
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_wider_delim separate
#'
readIEAEnergyPrices <- function(subtype = "RESID") {
  
  if (!file.exists("IEAEnergyPrices.rds")) {
    
    x1 <- read.table("IEA Energy Prices - Yearly.txt", header = FALSE)
    names(x1) <- c("region","fuel","sector","unit","period","value")
    x1[["region"]] <- factor(x1[["region"]])
    x1[["fuel"]] <- factor(x1[["fuel"]])
    x1[["sector"]] <- factor(x1[["sector"]])
    x1[["unit"]] <- factor(x1[["unit"]])
    x1[["period"]] <- as.numeric(x1[["period"]])
    x1[["value"]] <- as.numeric(x1[["value"]])
    x1 <- filter(x1,!is.na(value))
    x1 <- as.quitte(x1)
    
    x2 <- read.table("IEA Energy Prices Transport Fuels - Yearly.txt", header = FALSE)
    x2 <- separate(x2, V2, into = c("fuel", "sector"), sep = "\\.")
    names(x2) <- c("region","fuel","sector","unit","period","value")
    x2[["region"]] <- factor(x2[["region"]])
    x2[["fuel"]] <- factor(x2[["fuel"]])
    x2[["sector"]] <- factor(x2[["sector"]])
    x2[["unit"]] <- factor(x2[["unit"]])
    x2[["period"]] <- as.numeric(x2[["period"]])
    x2[["value"]] <- as.numeric(x2[["value"]])
    x2 <- filter(x2,!is.na(value))
    x2 <- as.quitte(x2)
    
    x3 <- read.table("IEA Energy Prices Other Products - Yearly.txt", header = FALSE, fill = TRUE)
    x3 <- separate(x3, V1, into = c("region", "fuel", "sector"), sep = "\\.")
    names(x3) <- c("region","fuel","sector","unit","period","value")
    x3[["region"]] <- factor(x3[["region"]])
    x3[["fuel"]] <- factor(x3[["fuel"]])
    x3[["sector"]] <- factor(x3[["sector"]])
    x3[["unit"]] <- factor(x3[["unit"]])
    x3[["period"]] <- as.numeric(x3[["period"]])
    x3[["value"]] <- as.numeric(x3[["value"]])
    x3 <- filter(x3,!is.na(value))
    x3 <- filter(x3,!is.na(period))
    x3 <- as.quitte(x3)
    
    x <- rbind(x1,x2,x3)
    
    saveRDS(object = x, file = "IEAEnergyPrices.rds")
  }
  
  x <- readRDS("IEAEnergyPrices.rds")
  
  if (subtype != "all") {
    x <- filter(x, x[["sector"]] == subtype)
  }
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "Costs",
                       type = "Energy prices",
                       filename = "IEA Energy Prices - Yearly.TXT",
                       `Indicative size (MB)` = 9,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "E3M"))
}
