#' readBoT
#'
#' Read in a csv file and convert it to a magpie object
#' The data has information about End-of-life vehicles of USA.
#'
#' @return magpie object with the requested output data about 
#' End-of-life vehicles of USA.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("BoT")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>%
#' @importFrom readxl read_excel
#'

readBoT <- function() {
  
  scrap <- read_excel("table_04_58q416.xlsx",
                      sheet = "4-58", range = "B2:AD3")
  
  scrap <- scrap %>% pivot_longer(cols=c("1970", "1975", "1980", "1985", "1990",
                                         C(1991:2014)),
                                  names_to='period',
                                  values_to='value')
  
  scrap["region"] <- "USA"
  
  y <- as.quitte(scrap) %>% as.magpie()
  y <- y*1000
  
  return(y)
  
}