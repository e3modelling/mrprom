#' readUN_GBR_LULUCF
#'
#' Read United Nations data for UK for LULUCF CO2 equivalent emissions.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- readSource("UN_GBR_LULUCF", convert = TRUE)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readUN_GBR_LULUCF <- function() {
  
  x <- read_excel("GBR-CRT-2024-V0.2-2022-20241224-142613_awaiting approval.xlsx",
                  sheet = "Table10s1")
  
  x <- x[c(7,70,71),-c(1,3,4,38)]
  
  names(x) <- x[1,]
  
  x <- x[-1,]
  
  names(x)[1] <- "variable"
  
  x[nrow(x) + 1, ] <- NA
  
  x[3,1] <- "Emissions|CO2|Land"
  
  x <- x %>% pivot_longer(!("variable"), names_to = "period", values_to = "value")
  x[["value"]] <- as.numeric(x[["value"]])
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  x[,,"Emissions|CO2|Land"] <- x[,,"Total CO2 equivalent emissions with LULUCF"] - x[,,"Total CO2 equivalent emissions without LULUCF"]
  x <- x[,,"Emissions|CO2|Land"] / 1000
  
  x <- as.quitte(x)
  
  x[["region"]] <- "GBR"
  x[["unit"]] <- "Mt CO2/yr"
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "UN data for CO2 equivalent emissions for LULUCF, UK",
                       type = "UN data for CO2 equivalent emissions for LULUCF, UK",
                       filename = "GBR-CRT-2024-V0.2-2022-20241224-142613_awaiting approval.xls",
                       `Indicative size (MB)` = 4.3,
                       dimensions = "2D",
                       unit = "MtCO2",
                       Confidential = "E3M"))
}
