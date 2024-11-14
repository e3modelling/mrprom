#' readEDGAR2
#'
#' Read GHG emissions of all world countries, 2024 Report from EDGAR.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- readSource("EDGAR2", convert = TRUE)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter select
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readEDGAR2 <- function() {
  
  x <- read_excel("EDGAR_2024_GHG_booklet_2024.xlsx",
                  sheet = "GHG_by_sector_and_country", range = "A1:BF4854")
  
  x <- select(x, -c("EDGAR Country Code"))
  
  x <- x %>% pivot_longer(!c("Substance", "Sector", "Country"), names_to = "period", values_to = "value")
  
  x <- as.data.frame(x)
  
  names(x)[3] <- "region"
  
  x[["region"]] <- toolCountry2isocode((x[["region"]]), mapping =
                                         c("EU27" = "EU27",
                                           "GLOBAL TOTAL" = "World",
                                           "Côte d’Ivoire" = "CIV",
                                           "Falkland Islands" = "FLK",
                                           "Faroes" = "FRO",
                                           "France and Monaco" = "FRA",
                                           "Israel and Palestine, State of" = "ISR",
                                           "Italy, San Marino and the Holy See" = "ITA",
                                           "Myanmar/Burma"  = "MMR",
                                           "São Tomé and Príncipe" = "STP",
                                           "Spain and Andorra" = "ESP",
                                           "Sudan and South Sudan" = "SDN",
                                           "Switzerland and Liechtenstein" = "CHE"))
  
  x <- filter(x, !is.na(x[["region"]]))
  x["unit"] <- "MtCO2"
  x["variable"] <- x["Substance"]
  x <- select(x, -c("Substance"))
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "GHG emissions",
                       type = "EDGAR GHG emissions",
                       filename = "EDGAR_2024_GHG_booklet_2024.XLSX",
                       `Indicative size (MB)` = 3.9,
                       dimensions = "2D",
                       unit = "Mt CO2eq/yr",
                       Confidential = "project"))
}
