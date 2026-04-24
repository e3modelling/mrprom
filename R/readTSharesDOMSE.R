#' readTSharesDOMSE
#'
#' The dataset contains targets shares for DOMSE.
#'
#' @return The read-in targets shares data into a magpie object
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("TSharesDOMSE")
#' }
#'
#' @importFrom dplyr filter select
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel
#' @importFrom quitte as.quitte
#'
readTSharesDOMSE <- function(subtype) {
  
  if (subtype == "SharesOP") {
    x <- read_excel("SharesOP.xlsx", col_names = TRUE)
    
    x <- select(x, -c("Sector", "Product", "Model"))
    
    x <- x %>% pivot_longer(!c("Region", "OP-Sector", "OP-Product"), names_to = "period", values_to = "value")
    
    x <- filter(x, !(`OP-Product` %in% c("OPEN-PROM Total", "Source Total")))
    
    x <- as.quitte(x) %>%
      interpolate_missing_periods(period = 2010:2100, expand.values = TRUE)
    
    x <- as.magpie(x)
  }
  
  if (subtype == "ProjectionsOP") {
    x <- read_excel("ProjectionsOP.xlsx", col_names = TRUE)
    
    x <- select(x, -c("Sector", "Product", "Model"))
    
    x <- x %>% pivot_longer(!c("Region", "OP-Sector", "OP-Product"), names_to = "period", values_to = "value")
    
    x <- filter(x, `OP-Product` %in% c("OPEN-PROM Total"))
    
    x <- as.quitte(x) %>%
      interpolate_missing_periods(period = 2010:2100, expand.values = TRUE)
    
    x <- as.magpie(x)
  }

  
  list(x = x,
       weight = NULL,
       description = c(category = "targets shares for DOMSE",
                       type = "targets shares for DOMSE",
                       filename = "SharesOP.xlsx",
                       `Indicative size (MB)` = 0.13,
                       dimensions = "3D",
                       unit = "shares",
                       Confidential = "E3M"))
  
}