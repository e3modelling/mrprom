#' readNavigateConsumption
#'
#' Read in NAVIGATE project model scenarios from IIASA.
#' The models are : 
#' COFFEE 1.3, REMIND-MAgPIE 3.0.0-4.5.0, IMAGE 3.2,PROMETHEUS V1,
#' GEM-E3_V2023,MESSAGEix-Materials,POLES NAVIGATE.
#' 
#' The scenarios are : 
#' NAV_Ind_NPi,NAV_Dem-NPi-ref.
#'
#' @param subtype Type of data that should be read.
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("NavigateConsumption")
#' }
#'
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#'
readNavigateConsumption <- function() {
  
  x <- readRDS("navigate.rds")
  x[["region"]] <- toolCountry2isocode((x[["region"]]))
  x <- filter(x, !is.na(x[["region"]]))
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  return(x)
}
