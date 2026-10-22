#' readProjectCarbonPrices
#'
#' Read Project Carbon Prices
#' The dataset contains carbon-price trajectories from several projects.
#' The csv file may be updated with new projects in the future.
#' Please have the values as US$2015/tCO2 otherwise convert them to [calcIEnvPolicies()].
#'
#' @return The read-in carbon price data into a magpie object
#'
#' @author Alexandros Tsimpoukis
#'
#' @examples
#' \dontrun{
#' a <- readSource("ProjectCarbonPrices")
#' }
#'
#' @importFrom tidyr pivot_longer
#' @importFrom utils read.csv
#' @importFrom quitte as.quitte
#'
readProjectCarbonPrices <- function() {
  
  x <- read.csv(file = "iEnvPolicies_allProjects.csv")
  
  names(x) <- sub("X", "", names(x))
  
  x <- x %>%
    pivot_longer(
      cols = `2010`:`2100`,
      names_to = "period",
      values_to = "value"
    )
  
  names(x) <- gsub("policy", "scenario", names(x))
  
  x <- as.quitte(x) %>% as.magpie()
  
  map <- toolGetMapping("regionmappingOPDEV5.csv", "regional", where = "mrprom")
  
  x <- toolAggregate(x, rel = map, weight = NULL, from = "Region.Code", to = "ISO3.Code", dim = 1)
  
  list(x = x,
       weight = NULL,
       description = c(category = "Costs",
                       type = "Carbon Price",
                       filename = "iEnvPolicies_allProjects.csv",
                       `Indicative size (MB)` = 0.152,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "project"))
  
}
