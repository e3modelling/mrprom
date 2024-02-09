#' readPRIMESPROCTECH
#'
#' Read PRIMES tech data and convert it to a magpie object
#'
#' @return The read-in data into magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- readSource("PRIMESPROCTECH")
#' }
#'
#' @importFrom dplyr select
#' @importFrom tidyr unite
#' @importFrom quitte as.quitte
#' @importFrom readxl read_excel

readPRIMESPROCTECH <- function() {
  
  x <- read_excel("PrindV6_process_techdata.xlsx",
                  sheet = "ProcessTech", range = "A2:AS291")

  df <- x[1 : 235, c(1, 2, 3, 4, 5, 36, 37, 38, 39, 6, 7, 10, 15)]
  
  names(df) <- sub("...1", "tech", names(df))
  names(df) <- sub("...2", "process", names(df))
  names(df) <- sub("...39", "EUR/t", names(df))
  names(df) <- sub("...36", "tech_sum", names(df))
    
  df <- as.magpie(df)
  
  return(df)
}