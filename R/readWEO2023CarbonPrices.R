#' readWEO2023CarbonPrices
#'
#' Read WEOCarbonPrices from IEA.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("WEO2023CarbonPrices")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readWEO2023CarbonPrices <- function() {
  
  x <- read.csv("CarbonPricesIEA.csv")
  x <- x[-1,]
  names(x) <- c("region",2030,2040,2050)
  
  x <- x %>% pivot_longer(!c("region"), names_to = "period", values_to = "value")
  
  x <- as.quitte(x)
  x[["unit"]] <- "US$2015/t CO2"
  
  x[["variable"]] <- "Price|Carbon"
  
  suppressWarnings({
    levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]), mapping =
                                                   c("Chile and Colombia " = "CHL",
                                                     "China " = "CHN",
                                                     "Korea " = "KOR",
                                                     "Canada " = "CAN"))
  })
  
  x <- filter(x, !is.na(x[["region"]]))
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  #converting US$2022 to US$2015)
  x <- x * 0.81
  
  x <- add_columns(x, addnm = "COL", dim = 1, fill = NA)
  
  x["COL",,] <- x["CHL",,]
  
  list(x = x,
       weight = NULL,
       description = c(category = "CarbonPrices",
                       type = "CarbonPrices from IEA",
                       filename = "CarbonPricesIEA.csv",
                       `Indicative size (MB)` = 0.01,
                       dimensions = "3D",
                       unit = "US$2015/t CO2",
                       Confidential = "E3M"))
}
