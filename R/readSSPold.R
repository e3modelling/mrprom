#' readSSPold
#'
#' Read in a csv file and convert it to a magpie object
#' The data has information about GDP|PPP and Population.
#'
#' @return magpie object object with the requested output GDP|PPP and Population
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("SSPold")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom tidyr drop_na pivot_longer
#' @importFrom dplyr %>% mutate filter select
#' @importFrom readxl read_excel
#' @importFrom utils read.csv
#'

readSSPold <- function() {
  
  # SspDb_country_data
  x <- read.csv("SspDb_country_data_2013-06-12.csv")
  
  x <- x %>% pivot_longer(c(6 : 24), names_to = "period", values_to = "value")
  
  x[["period"]] <- gsub("X","", x[["period"]])
  
  x <- as.quitte(x)
  x <- as.magpie(x) 
  x <- toolCountryFill(x)
  x <- x[as.character(getISOlist()), , ]
  
  list(x = x,
       weight = NULL,
       description = c(data_id = "Population",
                       category = "GDP|PPP and Population",
                       filename = "SspDb_country_data_2013-06-12.csv",
                       `Indicative size (MB)` = 0.639,
                       dimensions = "4D",
                       unit = "varius",
                       Confidential = "E3M"))
  
}
