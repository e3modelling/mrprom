#' readIEA_Energy_Projections_Balances
#'
#' Read in Energy_Projections_Balances from International Energy Agency.
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
  x <- as.quitte(x)
  x["unit"] <- "Mtoe"
  x <- as.magpie(x)
  x <- toolCountryFill(x)
  x <- collapseDim(x, dim = c(3.1, 3.3))
  return(x)
}
