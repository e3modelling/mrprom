#' readEU_RefScen2020
#'
#' Read in data from CarbonPrice_fromReportFig8.
#' The dataset contains carbon price data for the EU Reference Scenario 2020.
#'
#' @return The read-in carbon price data into a magpie object
#'
#' @author Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- readSource("EU_RefScen2020", convert = TRUE)
#' }
#'
#' @importFrom utils read.csv
#'
readEU_RefScen2020 <- function() {

  x <- read.csv(file = "CarbonPrice_fromReportFig8.csv")

  names(x) <- sub("X", "", names(x))

  x <- as.magpie(x)
  getSets(x)[2] <- "Year"
  
  list(x = x,
       weight = NULL,
       description = c(category = "Cost",
                       type = "Carbon Price",
                       filename = "CarbonPrice_fromReportFig8.csv",
                       `Indicative size (MB)` = 0.001,
                       dimensions = "4D",
                       unit = "varius",
                       Confidential = "project"))

}
