#' readClimateWatch
#'
#' Read ClimateWatch data : model meta information, scenario meta information
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
#' a <- readSource("ClimateWatch", subtype = "AEO_2017_Timeseries data")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter %>% select
#' @importFrom readxl read_excel
#'

readClimateWatch <- function(subtype = "AEO_2017_Timeseries data") {
  
  if (subtype == "AEO_2017_Timeseries data") {
    x <- read_excel("AEO_2017.xlsx",
                    sheet = "AEO_2017_Timeseries data")
    
    names(x) <- sub("Unit of Entry", "unit", names(x))
    names(x) <- sub("ESP Indicator Name", "variable", names(x))
    
    x <- as.quitte(x) %>% as.magpie()
  }
  
  if (subtype == "GC") {
    x <- read_excel("GC.xlsx",
                    sheet = "GC_Timeseries data")
    
    names(x) <- sub("Unit of Entry", "unit", names(x))
    names(x) <- sub("ESP Indicator Name", "variable", names(x))
    
    x <- as.quitte(x) %>% as.magpie()
  }
  
  if (subtype == "GCAM") {
    x <- read_excel("GCAM.xlsx",
                    sheet = "GCAM_Timeseries data")
    
    names(x) <- sub("Unit of Entry", "unit", names(x))
    names(x) <- sub("ESP Indicator Name", "variable", names(x))
    
    x <- as.quitte(x) %>% as.magpie()
  }
  
  if (subtype == "GMM") {
    x <- read_excel("GMM.xlsx",
                    sheet = "GMM_Timeseries data")
    
    names(x) <- sub("Unit of Entry", "unit", names(x))
    names(x) <- sub("ESP Indicator Name", "variable", names(x))
    
    x <- as.quitte(x) %>% as.magpie()
  }
  
  if (subtype == "IEO") {
    x <- read_excel("IEO.xlsx",
                    sheet = "IEO_Timeseries data")
    
    names(x) <- sub("Unit of Entry", "unit", names(x))
    names(x) <- sub("ESP Indicator Name", "variable", names(x))
    
    x <- as.quitte(x) %>% as.magpie()
  }
  
  if (subtype == "UNFCCC") {
    x <- read_excel("UNFCCC Biennial Reports.xlsx",
                    sheet = "UNFCCC Biennial Reports_Timeser")
    
    names(x) <- sub("Unit of Entry", "unit", names(x))
    names(x) <- sub("ESP Indicator Name", "variable", names(x))
    
    x <- x %>% pivot_longer(!c("Model","Scenario","Region","variable","unit"), names_to = "period", values_to = "value")
    
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