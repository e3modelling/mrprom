#' readIEA_Energy_Projections_Indicators
#'
#' Read in IEA_Energy_Projections_Indicators from International Energy Agency.
#' The period is: 2030, 2040 and 2050 based on Business as usual.
#'
#' @param subtype Type of data that should be read.
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_Energy_Projections_Indicators")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter select
#' @importFrom quitte as.quitte
#'
readIEA_Energy_Projections_Indicators <- function() {
  
  x <- read.csv("OECD.IEA,SLTINDICEXTEND,1.0,filtered,2025-12-04 10-52-30.csv", header = TRUE)
  x <- select(x, c("COUNTRY","Indicator","TIME_PERIOD","OBS_VALUE","UNIT","SCENARIO"))
  names(x) <- c("region", "flow", "period", "value", "unit","scenario")
  x[["region"]] <- factor(x[["region"]])
  x[["flow"]] <- factor(x[["flow"]])
  x[["scenario"]] <- factor(x[["scenario"]])
  x[["unit"]] <- factor(x[["unit"]])
  x[["period"]] <- as.numeric(x[["period"]])
  x[["value"]] <- as.numeric(x[["value"]])
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "IEA_Energy_Projections_Indicators",
                       type = "IEA_Energy_Projections_Indicators",
                       filename = "OECD.IEA,SLTINDICEXTEND,1.0,filtered,2025-12-04 10-52-30.csv",
                       `Indicative size (MB)` = 2.4,
                       dimensions = "4D",
                       unit = "various",
                       Confidential = "E3M"))
}
