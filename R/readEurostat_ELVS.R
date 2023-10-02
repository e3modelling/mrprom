#' readEurostat_ELVS
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
#' a <- readSource("Eurostat_ELVS")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>%
#' @importFrom readxl read_excel
#'

readEurostat_ELVS <- function() {
  
  scrap <- read_excel("SE_End-of-life_vehicle_statistics_2023-07.xlsx",
                   sheet = "Table 1", range = "B4:O35")
  
  percentage_reuse <- read_excel("SE_End-of-life_vehicle_statistics_2023-07.xlsx",
                                 sheet = "Table 2",range = "B4:O35")
  
  scrap <- scrap %>% 
    mutate(across(c("2008", "2009", "2010", "2011", "2012","2013", "2014", "2015",
                    "2016", "2017","2018", "2019", "2020"),
                  ~ as.numeric(stringr::str_remove(., "[A-Za-z]"))))
  
  percentage_reuse <- percentage_reuse %>% 
    mutate(across(c("2008", "2009", "2010", "2011", "2012","2013", "2014", "2015",
                    "2016", "2017","2018", "2019", "2020"),
                  ~ as.numeric(stringr::str_remove(., "[A-Za-z]"))))
  
  cars_reuse <- scrap[,2:length(scrap)]*(percentage_reuse[,2:length(percentage_reuse)]/100)
  
  cars_reuse <- cbind(scrap[,1], cars_reuse)
  
  cars_reuse <- cars_reuse[-1,]
  names(cars_reuse)[1] <- "region"
  
  cars_reuse[,"region"] <- toolCountry2isocode((cars_reuse[,"region"]), mapping = c("Croatia (²)" = "HRV",
                                                                                          "Malta (³)" = "MLT",
                                                                                          "Iceland (³)" = "ISL"))
  
  
  cars_reuse <- cars_reuse %>% pivot_longer(cols=c(C(2008:2020)),
                                            names_to='period',
                                            values_to='value')
  
  
  y <- as.quitte(cars_reuse) %>% as.magpie()
  
  a1 <- readSource("IRF", subtype = "passenger-cars-in-use")
  a2 <- readSource("IRF", subtype = "total-vehicles-in-use")
  
  a1 <- a1[,Reduce(intersect, list(getYears(a1),getYears(a2))),]
  a2 <- a2[,Reduce(intersect, list(getYears(a1),getYears(a2))),]
  
  a <- a1/a2
  a <- a/a2
  countries <- unique(cars_reuse[,"region"])
  a <- a[countries, , ]
  
  a <- a[,Reduce(intersect, list(getYears(a),getYears(y))),]
  y <- y[,Reduce(intersect, list(getYears(a),getYears(y))),]
  
  x <- a*y
  
  getNames(x) <- "percentage_of_reuse_pc"
  getSets(x) <- c("region", "period", "unit")
  
  return(x)
  
}