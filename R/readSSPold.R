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
  myColTypes <- c(rep.int("text", 5), rep.int("numeric", 31))
  x <- read_xlsx("ssp_basic_drivers_release_3.2.beta_full.xlsx",
                 sheet = "data",
                 col_types = myColTypes,
                 progress = FALSE)
  
  x <- x %>% pivot_longer(c(6 : 36), names_to = "period", values_to = "value")
  
  x <- filter(x, period %in% c(2010 : 2025))
  
  x <- as.quitte(x)
  
  levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]),
                                               mapping =
                                                 c("WORLD" = "GLO"))
  
  x <- filter(x, !is.na(x[["region"]]))
  
  x <- as.magpie(x)
  
  x <- as.magpie(x) 
  x <- toolCountryFill(x)
  x <- x[as.character(getISOlist()), , ]
  
  list(x = x,
       weight = NULL,
       description = c(category = "demographics",
                       type = "GDP|PPP and Population",
                       filename = "ssp_basic_drivers_release_3.2.beta_full.xlsx",
                       `Indicative size (MB)` = 0.639,
                       dimensions = "4D",
                       unit = "varius",
                       Confidential = "E3M"))
  
}
