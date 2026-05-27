#' readUNFCCC
#'
#' Read in UNFCCC country-submitted greenhouse gas emissions data until 2024-07-05
#'
#' @param subtype Type of data that should be read.
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- readSource("UNFCCC", subtype = "2.  Industrial Processes and Product Use")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
readUNFCCC <- function(subtype = "2.  Industrial Processes and Product Use") {
  
  x <- readRDS("all.parquet.rds")
  
  names(x)[names(x) == "party"] <- "region"
  names(x)[names(x) == "measure"] <- "Variable"
  names(x)[names(x) == "numberValue"] <- "value"
  names(x)[names(x) == "year"] <- "period"
  
  if (subtype != "all") {
    x <- filter(x, x[["category"]] == subtype)
  }
  
  suppressWarnings({
    levels(x[["region"]]) <- toolCountry2isocode((levels(x[["region"]])))
  })
  
  x <- filter(x, !is.na(x[["region"]]))
  suppressWarnings({
    x <- as.quitte(x) %>% as.magpie()
  })

  list(x = x,
       weight = NULL,
       description = c(category = "Greenhouse Gas Emissions",
                       type = "UNFCCC Greenhouse Gas Emissions",
                       filename = "all.parquet.rds",
                       `Indicative size (MB)` = 38,
                       dimensions = "3D",
                       unit = "varius",
                       Confidential = "E3M"))

}
