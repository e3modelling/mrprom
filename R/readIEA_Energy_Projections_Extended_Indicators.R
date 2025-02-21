#' readIEA_Energy_Projections_Extended_Indicators
#'
#' Read in IEA_Energy_Projections_Extended_Indicators from International Energy Agency.
#'
#' The “scenario” dimension includes: 
#' business as usual, stated policies, achieving national targets, achieving 
#' defined outcomes and other and for period is 2030, 2040 and 2050.
#' 
#' The flows are corresponding to the energy balance.
#' 
#' @param subtype Type of data that should be read.
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_Energy_Projections_Extended_Indicators", subtype = "Total energy supply (TJ)")
#' }
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_wider_delim pivot_longer
#'
readIEA_Energy_Projections_Extended_Indicators <- function(subtype = "Total energy supply (TJ)") {
  
  if (!file.exists("Energy_Projections_Extended_Indicators.rds")) {
    x <- read.csv2("Energy_Projections_Extended_indicators.csv")
    x <- data.frame(x[-1, ])
    x <- separate_wider_delim(x, cols = "x..1...", delim = ",", names = c("region","scenario","flow", c(seq(1960 , 2010, 10), 2015 : 2021, 2030, 2040, 2050)))
    x <- x %>% pivot_longer(!c("region", "flow", "scenario"), names_to = "period", values_to = "value")
    x[["region"]] <- factor(x[["region"]])
    x[["flow"]] <- factor(x[["flow"]])
    x[["scenario"]] <- factor(x[["scenario"]])
    x[["period"]] <- as.numeric(x[["period"]])
    x[["value"]] <- as.numeric(x[["value"]])
    
    saveRDS(object = x, file = "Energy_Projections_Extended_Indicators.rds")
  }
  
  x <- readRDS("Energy_Projections_Extended_Indicators.rds")
  
  if (subtype != "all") {
    x <- filter(x, x[["flow"]] == subtype)
  }
  x["unit"] <- "varius"
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "IEA_Energy_Projections_Extended_Indicators",
                       type = "Energy Projections Extended Indicators",
                       filename = "Energy_Projections_Extended_indicators.csv",
                       `Indicative size (MB)` = 1.4,
                       dimensions = "3D",
                       unit = "MtCO2",
                       Confidential = "E3M"))
}
