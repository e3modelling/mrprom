#' readEUC
#'
#' Read in a XLSX file and convert it to a dataframe.
#' The data has information about emission factors from the EUROPEAN COMMISSION.
#'
#' @return dataframe with the requested output data about emission factors from
#' the EUROPEAN COMMISSION.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("EUC")
#' }
#' 
#' @importFrom readxl read_excel
#'

readEUC <- function() {
  
  x <- read_excel("Emission_Factor_25092024.xlsx")
  x <- x[, c(1 : 3)]
  names(x) <- x[1,]
  x <- x[-1, ]
  x["unit"] <- names(x[2])
  names(x) <- gsub("tCO2/toe", "value", names(x))
  
  x[["value"]] <- as.numeric(x[["value"]])
  x <- as.data.frame(x)
  
  list(x = x,
       weight = NULL,
       unit = "tCO2/toe",
       class = "data.frame",
       description = "Emission factors from the EUROPEAN COMMISSION")
}
