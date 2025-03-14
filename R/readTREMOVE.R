#' readTREMOVE
#'
#' Read TREMOVE data : model meta information, scenario meta information
#'
#' @param subtype Type of data that should be read. The type is referring to the
#' excel and converts it to a magpie object.
#' 
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("TREMOVE", subtype = "AEO_2017_Timeseries data")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter %>% select
#' @importFrom readxl read_excel
#'

readTREMOVE <- function(subtype = "AEO_2017_Timeseries data") {
  
  if (subtype == "AEO_2017_Timeseries data") {
    x <- read_excel("AEO_2017.xlsx",
                    sheet = "AEO_2017_Timeseries data")
    
    names(x) <- sub("Unit of Entry", "unit", names(x))
    names(x) <- sub("ESP Indicator Name", "variable", names(x))
    
    x <- as.quitte(x) %>% as.magpie()
  }
  

  
  list(x = x,
       weight = NULL,
       description = c(category = "model meta information, scenario meta information",
                       type = "model meta information, scenario meta information",
                       filename = "AEO_2017.xlsx",
                       `Indicative size (MB)` = 2.7,
                       dimensions = "3D",
                       unit = "varios",
                       Confidential = "E3M"))
}