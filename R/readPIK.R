#' readPIK
#'
#' Read PIK_CO2 emissions from PIK.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- readSource("PIK", convert = TRUE)
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readPIK <- function() {
  
  x <- read.csv("CW_HistoricalEmissions_PIK.csv")
  
  x <- x %>% pivot_longer(!c("country", "sector", "gas", "Source"), names_to = "period", values_to = "value")
  names(x)[1] <- "region"
  names(x)[2] <- "variable"
  
  x["period"] <- gsub("X", "", as.factor(x[["period"]]))
  x <- x[, c(1:3, 5, 6)]
  
  x <- filter(x, !is.na(x[["region"]]))
  x["unit"] <- "MtCO2"
  x <- as.quitte(x)
  x <- as.magpie(x)

  list(x = x,
       weight = NULL,
       description = c(category = "CO2 Emissions",
                       type = "CO2 Emissions",
                       filename = "CW_HistoricalEmissions_PIK.csv",
                       `Indicative size (MB)` = 8,
                       dimensions = "3D",
                       unit = "MtCO2",
                       Confidential = "E3M"))
}
