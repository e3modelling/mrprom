#' readWSA
#'
#' Read WSA.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("WSA")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readWSA <- function() {
  setwd("C:/Users/sioutas/Ricardo Plc/Global Integrated Assessment Models - Documents/Work/PROMETHEUS Model/madratverse/sources/WSA")
  
  x <- read.csv("iProdHistIS.csv")
  
  names(x) <- sub("^X", "", names(x))
  x <- dplyr::rename(
    x,
    variable = dummy.1,
    region = dummy
  )
  
  x <- x %>% pivot_longer(!c("region", "variable"), names_to = "period", values_to = "value")
  
  x <- filter(x, !is.na(x[["region"]]))
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "WSA",
                       type = "WSA",
                       filename = "iProdHistIS.csv",
                       `Indicative size (MB)` = 0.08,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "E3M"))
}
