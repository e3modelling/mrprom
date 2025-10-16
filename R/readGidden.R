#' readGidden
#'
#' Read Gidden Gt CO2 emissions.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("Gidden", convert = TRUE)
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer separate
#' @importFrom quitte as.quitte
#'
readGidden <- function() {
  
  x <- read_excel("gidden_et_al_2025_supplemental_data.xlsx", sheet = "S5",, col_names = FALSE)
  
  x <- x[-c(3,197,198,233),-c(2,3)]
  
  names(x) <- c("region","Technical Potential|Offshore","Technical Potential|Onshore","Technical Potential|Total",
                "Planetary Limit|Offshore","Planetary Limit|Onshore","Planetary Limit|Total",
                "Potential in Basins with Oil & Gas Infrastructure|Offshore","Potential in Basins with Oil & Gas Infrastructure|Onshore",
                "Potential in Basins with Oil & Gas Infrastructure|Total")
  
  x <- x[-c(1,2),]
  
  x <- x %>% pivot_longer(!c("region"), names_to = "variable", values_to = "value")
  
  x <- x %>% separate(variable, into = c("variable", "type"), sep = "\\|")
  
  x[["value"]] <- as.numeric(x[["value"]])
  
  x <- as.quitte(x)
  
  x[["unit"]] <- "Gt CO2"
  x[["period"]] <- "2025"
  x[["model"]] <- "Gidden"

  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "Gt CO2 emissions",
                       type = "Gidden Gt CO2 emissions",
                       filename = "gidden_et_al_2025_supplemental_data.xlsx",
                       `Indicative size (MB)` = 0.62,
                       dimensions = "3D",
                       unit = "Gt CO2",
                       Confidential = "E3M"))
}
