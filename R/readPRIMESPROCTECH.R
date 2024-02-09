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
  
  names(df) <- sub("...1", "process", names(df))
  names(df) <- sub("...2", "tech", names(df))
  names(df) <- sub("...39", "EUR/t", names(df))
  names(df) <- sub("...36", "tech_sum", names(df))
  names(df) <- sub("Inv_costprocess5", "Inv_cost_ORD", names(df))
  names(df) <- sub("Lifettech020", "Lifetime2020", names(df))
  
  x[, 3] <- as.numeric(unlist(x[, 3]))
  x[, 4] <- as.numeric(unlist(x[, 4]))
  x[, 5] <- as.numeric(unlist(x[, 5]))
  x[, 6] <- as.numeric(unlist(x[, 6]))
    
  x <- as.magpie(x)
  
  return(x)
}