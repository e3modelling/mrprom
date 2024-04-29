#' readIEA_PRICES
#'
#' Read in energy prices from International Energy Agency.
#'
#' @param subtype Type of data that should be read.
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_PRICES", subtype = "ELECTR")
#' }
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_wider_delim
#'
readIEA_PRICES <- function(subtype = "ELECTR") {
  
  if (!file.exists("IEA_PRICES.rds")) {
    x <- read.csv2("IEA_ENERGY_PRICES.csv")
    x <- data.frame(x[-1, ])
    
    x <- separate_wider_delim(x, cols = "x..1...", delim = ",", names = c("COUNTRY","PRODUCT","SECTOR","TIME", "VALUE"))
    names(x) <- c("region", "product", "sector", "period", "value")
    x[["region"]] <- factor(x[["region"]])
    x[["product"]] <- factor(x[["product"]])
    x[["sector"]] <- factor(x[["sector"]])
    x[["period"]] <- as.numeric(x[["period"]])
    x[["value"]] <- as.numeric(x[["value"]])
    saveRDS(object = x, file = "IEA_PRICES.rds")
  }
  
  x <- readRDS("IEA_PRICES.rds")
  
  levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]), mapping =
                                                 c("Bolivarian Republic of Venezuela" = "VEN",
                                                   "China (P.R. of China and Hong Kong, China)" = "CHA",
                                                   "Kingdom of Eswatini" = "SWZ",
                                                   "Republic of the Congo" = "COG",
                                                   "Republic of Turkiye" = "TUR",
                                                   "IEAFAMILY" = "GLO"))
  x <- filter(x, !is.na(x[["region"]]))
  if (subtype != "all") {
    x <- filter(x, x[["product"]] == subtype)
  }
  x <- as.quitte(x)
  x["unit"] <- "2015 USD"
  x <- as.magpie(x)
  x <- toolCountryFill(x)
  x <- collapseDim(x, dim = c(3.1, 3.2, 3.3))
  return(x)
}
