#' readEuropeanHydrogenPrices
#'
#' Read EuropeanHydrogenPrices from European Hydrogen Observatory.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("EuropeanHydrogenPrices", convert = TRUE)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readEuropeanHydrogenPrices <- function() {
  
  x1 <- read_excel("Hydrogen production costs 2022.xlsx")
  
  x2 <- read_excel("Hydrogen production costs 2023.xlsx")
  
  names(x1) <- c("region", "variable" ,"cost","value", "period")
  names(x2) <- names(x1)
  
  x <- rbind(x1, x2)
  
  x <- as.quitte(x)
  
  x[["unit"]] <- "€/kg"
  
  suppressWarnings({
    x[["region"]] <- toolCountry2isocode((x[["region"]]), mapping =
                                           c("Luxemburg" = "LUX"))
  })
  
  x <- as.quitte(x)
  x <- x %>% drop_na()
  
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "EuropeanHydrogen Prices from European Hydrogen Observatory",
                       type = "Hydrogen production costs 2022",
                       filename = "Hydrogen production costs 2022.xlsx",
                       `Indicative size (MB)` = 0.5,
                       dimensions = "3D",
                       unit = "€/kg",
                       Confidential = "E3M"))
}
