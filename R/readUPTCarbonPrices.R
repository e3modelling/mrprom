#' readUPTCarbonPrices
#'
#' Read UPT Carbon Prices
#' The dataset contains carbon price data.
#'
#' @return The read-in carbon price data into a magpie object
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("UPTCarbonPrices")
#' }
#'
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom utils read.csv
#' @importFrom quitte as.quitte
#'
readUPTCarbonPrices <- function() {
  
  x <- read.csv(file = "iEnvPolicies_allScenarios.csv")
  
  names(x) <- sub("X", "", names(x))
  
  x <- x %>%
    pivot_longer(
      cols = `2010`:`2100`,
      names_to = "period",
      values_to = "value"
    )
  
  names(x) <- gsub("dummy.1", "scenario", names(x))
  names(x) <- gsub("dummy", "region", names(x))
  x <- filter(x, scenario != "exogCV_Calib")
  
  x <- as.quitte(x) %>% as.magpie()
  
  map <- toolGetMapping("regionmappingOPDEV5.csv", "regional", where = "mrprom")
  
  x <- toolAggregate(x, rel = map, weight = NULL, from = "Region.Code", to = "ISO3.Code", dim = 1)
  
  list(x = x,
       weight = NULL,
       description = c(category = "Costs",
                       type = "Carbon Price",
                       filename = "iEnvPolicies_allScenarios.csv",
                       `Indicative size (MB)` = 0.152,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "project"))
  
}