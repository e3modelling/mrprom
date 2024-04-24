#' readIEA_Energy_Projections_Extended_Indicators
#'
#' Read in IEA_Energy_Projections_Extended_Indicators from International Energy Agency.
#'
#' @param subtype Type of data that should be read.
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_Energy_Projections_Extended_Indicators", subtype = "TES")
#' }
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_wider_delim pivot_longer
#'
readIEA_Energy_Projections_Extended_Indicators <- function(subtype = "TES") {
  
  if (!file.exists("Energy_Projections_Extended_Indicators.rds")) {
    x <- read.csv2("Energy_Projections_Extended_indicators.csv")
    x <- data.frame(x[-1, ])
    x <- separate_wider_delim(x, cols = "x..1...", delim = ",", names = c("region","flow","scenario", c(seq(1960 , 2010, 10), 2015 : 2021, 2030, 2040, 2050)))
    x <- x %>% pivot_longer(!c("region", "flow", "scenario"), names_to = "period", values_to = "value")
    x[["region"]] <- factor(x[["region"]])
    x[["flow"]] <- factor(x[["flow"]])
    x[["scenario"]] <- factor(x[["scenario"]])
    x[["period"]] <- as.numeric(x[["period"]])
    x[["value"]] <- as.numeric(x[["value"]])
    
    saveRDS(object = x, file = "Energy_Projections_Extended_Indicators.rds")
  }
  
  x <- readRDS("Energy_Projections_Extended_Indicators.rds")
  
  levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]), mapping =
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
  x["unit"] <- "varius"
  x <- as.quitte(x)
  x <- as.magpie(x)
  x <- toolCountryFill(x)
  return(x)
}
