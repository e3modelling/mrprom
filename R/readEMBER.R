#' readEMBER
#'
#' Read EMBER capacity and production.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("EMBER", convert = TRUE)
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter select
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readEMBER <- function() {
  
  x <- read.csv("yearly_full_release_long_format.csv")
  
  x <- select(x,c("ISO.3.code","Year","Category","Subcategory","Variable","Unit","Value"))
  
  x <- filter(x,Category %in% c("Capacity","Electricity generation") & Subcategory == "Fuel" & Unit %in% c("TWh","GW"))
  
  x <- select(x,-c("Subcategory"))
  
  names(x) <- sub("ISO.3.code", "region", names(x))
  
  x <- x[!is.na(x[["region"]]), ]
  x <- x[x[["region"]] != "", ]
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "EMBER capacity and production",
                       type = "EMBER capacity and production",
                       filename = "yearly_full_release_long_format.csv",
                       `Indicative size (MB)` = 47,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "E3M"))
}
