#' readEMO
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("EMO")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readEMO <- function() {
  
  x <- read.csv("EMO trends_yearly_reduced_all.csv")
  
  x <- select(x, -c(uni_0, uni_3, uni_4, uni_13, uni_9)) %>%
    rename(region = uni_1, period = uni_2, variable = uni_5, unit = uni_12, extra = uni_10)
  
  x <- filter(x, !(period %in% c("2028-2040", "2036-2050", "2028-2050")))
  
  x <- filter(x, variable %in% c("Price","Demand", "Installed Capacity",
                                 "Generation", "Utilization"))
  
  x <- as.quitte(x)
  
  levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]), mapping =
                                                 c("XK" = "Kosovo"))
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "EMO data for electricity sector",
                       type = "EMO data for electricity sector",
                       filename = "trends_yearly_reduced_all.csv",
                       `Indicative size (MB)` = 60,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "E3M"))
}
