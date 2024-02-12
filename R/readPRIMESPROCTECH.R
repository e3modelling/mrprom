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
#' @importFrom quitte as.quitte
#' @importFrom readxl read_excel

readPRIMESPROCTECH <- function() {
  
  x <- read_excel("PrindV6_process_techdata.xlsx",
                  sheet = "ProcessTech", range = "A2:AS291")

  df <- x[1 : 235, c(1, 2, 3, 4, 5, 36, 37, 38, 39, 6, 7, 10, 15)]
  
  names(df) <- sub("...1", "variable", names(df))
  names(df) <- sub("...2", "process", names(df))
  names(df) <- sub("...39", "EUR/t", names(df))
  names(df) <- sub("...36", "variable_sum", names(df))
  names(df) <- sub("Lifetprocess020", "Lifetime2020", names(df))
  names(df) <- sub("Inv_costvariable5", "Inv_cost_ORD", names(df))
    
  df <- as.magpie(df)
  
  return(df)
}