#' readEurostatHDD
#'
#' Read HDD from Eurostat
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("EurostatHDD")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter rename select mutate across
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom quitte as.quitte
#' @importFrom readxl read_excel
#'
readEurostatHDD <- function() {
  
  x <- read_excel("nrg_chdd_a__custom_20828456_spreadsheet.xlsx", sheet = "Sheet 1", col_names = TRUE, skip = 8)
  
  x <- x[-c(1,31:33),]  %>% rename(region = TIME)
  
  x <- select(x, - "...94")
  
  x <- x %>%
    mutate(across(-region, as.numeric)) %>%
    pivot_longer(!region, names_to = "period", values_to = "value")
  
  x <- drop_na(x)
  
  x[["unit"]] <- "degree*days/yr"
  x[["variable"]] <- "HDD"
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "HDD",
                       type = "HDD",
                       filename = "nrg_chdd_a__custom_20828456_spreadsheet.xlsx",
                       `Indicative size (MB)` = 0.43,
                       dimensions = "3D",
                       unit = "degree*days/yr",
                       Confidential = "E3M"))
}
