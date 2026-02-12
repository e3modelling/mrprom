#' readIndustrialProcessesEmissionsIEAPrimes
#'
#' Read IndustrialProcessesEmissions of all world countries from IEA Primes
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IndustrialProcessesEmissionsIEAPrimes")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter select distinct
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#' @importFrom readxl read_excel excel_sheets
#'
readIndustrialProcessesEmissionsIEAPrimes <- function() {
  
  namesSheets <- readxl::excel_sheets("IndustrialProcessesEmissions.xlsx")
  
  sheets <- lapply(excel_sheets("IndustrialProcessesEmissions.xlsx"), read_excel, path = "IndustrialProcessesEmissions.xlsx")
  
  x <- NULL
  for (i in 1 : length(sheets)) {
    y <- as.data.frame(sheets[i])
    colnames(y) <- sub("^X", "", colnames(y))
    y <- y[,-2]
    names(y) <- gsub("OPEN.PROM.Regions","region",names(y))
    y <- y %>% pivot_longer(!"region", names_to = "period", values_to = "value")
    y[["scenario"]] <- namesSheets[i]
    y <- as.quitte(y)
    y[["variable"]] <- "Emissions|CO2|Industrial Processes"
    y[["unit"]] <- "Mt CO2/yr"
    x <- rbind(x, y)
  }

  x <- as.quitte(x)
  x <- as.magpie(x)
  
  x[is.na(x)] <- 0
  
  list(x = x,
       weight = NULL,
       description = c(category = "Greenhouse Gas Emissions",
                       type = "Emissions|CO2|Industrial Processes",
                       filename = "Emissions|CO2|Industrial Processes.xlsx",
                       `Indicative size (MB)` = 0.101,
                       dimensions = "2D",
                       unit = "Mt CO2/yr",
                       Confidential = "project"))
}
