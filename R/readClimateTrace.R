#' readClimateTrace
#'
#' Read Emissions|CO2|AFOLU of all world countries from ClimateTrace
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("ClimateTrace", convert = TRUE)
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter select distinct
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readClimateTrace <- function() {
  
  x1 <- read.csv("trace_data_since_2015_to_2023_1.csv")
  x2 <- read.csv("trace_data_since_2015_to_2023_2.csv")
  x3 <- read.csv("trace_data_since_2015_to_2023_3.csv")
  x4 <- read.csv("trace_data_since_2015_to_2023_4.csv")
  
  x <- rbind(x1,x2,x3,x4)
  
  x <- x[,1:4]
  
  names(x) <- c("region","variable","period","value")
  
  x <- mutate(x, value = sum(value, na.rm = TRUE), .by = c("period","region"))
  
  x[["variable"]] <- "Emissions|CO2|AFOLU"
  
  x <- distinct(x)
  
  x <- as.quitte(x)
  
  x <- filter(x, !is.na(x[["region"]]))
  
  x[["value"]] <- x[["value"]] / 10^6
  
  x[["unit"]] <- "Mt CO2/yr"
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  x <- x[as.character(getISOlist()), , ]
  
  x[is.na(x)] <- 0
  
  list(x = x,
       weight = NULL,
       description = c(category = "Greenhouse Gas Emissions",
                       type = "Emissions|CO2|AFOLU",
                       filename = "trace_data_since_2015_to_2023_1.csv",
                       `Indicative size (MB)` = 0.4,
                       dimensions = "2D",
                       unit = "Mt CO2/yr",
                       Confidential = "project"))
}
