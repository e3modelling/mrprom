#' readBoT
#'
#' Read in a csv file and convert it to a magpie object
#' The data has information about End-of-life passenger car vehicles.
#'
#' @return magpie object with the requested output data about 
#' End-of-life passenger car vehicles.
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
  
  getNames(y) <- "percentage_of_reuse_pc"
  getSets(y) <- c("region", "period", "unit")
  
  
  a1 <- readSource("IRF", subtype = "passenger-cars-in-use")
  a2 <- readSource("IRF", subtype = "total-vehicles-in-use")
  
  a1 <- a1[,Reduce(intersect, list(getYears(a1),getYears(a2))),]
  a2 <- a2[,Reduce(intersect, list(getYears(a1),getYears(a2))),]
  
  a <- a1/a2
  a <- a/a2
  a <- a["USA", , ]
  
  a <- a[,Reduce(intersect, list(getYears(a),getYears(y))),]
  y <- y[,Reduce(intersect, list(getYears(a),getYears(y))),]
  
  x <- a*y
  
  getNames(x) <- "percentage_of_reuse_pc"
  getSets(x) <- c("region", "period", "unit")
  
}