#' readEDGAR2
#'
#' Read GHG emissions of all world countries, 2024 Report from EDGAR.
#' Sheet names include: GHG_totals_by_country, GHG_by_sector_and_country,
#' GHG_per_GDP_by_country, GHG_per_capita_by_country and LULUCF_macroregions.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios, Alexandros Tsimpoukis
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
#' @importFrom magclass as.magpie
#' 
readEDGAR2 <- function(subtype = "GHG_totals_by_country") {

  x <- read_excel("EDGAR_2024_GHG_booklet_2024.xlsx",
                  sheet = subtype)

  if (subtype == "GHG_by_sector_and_country") {
    columns <- c("Substance", "Sector", "Country")
  } else if (subtype == "LULUCF_macroregions") {
    stop("Reading this sheet is not supported yet.")
  } else {
    columns <- c("Country")
  }
  
  x <- select(x, -c("EDGAR Country Code"))
  
  x <- x %>% pivot_longer(!columns, names_to = "period", values_to = "value")
  
  x <- as.data.frame(x)
  
  x <- x %>% rename(region = Country)
  
  suppressWarnings({
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
  })
  
  # Filter and Assign Units using Logic
  # Instead of x[, 1] and x[, 6], we use the actual column names
  x <- x %>%
    filter(!is.na(region)) %>%
    mutate(unit = case_when(
      # If the first column (Substance or Sector) contains "GWP"
      grepl("^GWP", .data[[names(.)[1]]]) ~ "1",
      TRUE ~ "Mt CO2-equiv/yr"
    ))
  
  if (subtype == "GHG_by_sector_and_country") {
    x <- x %>%
      rename(variable = Substance)
  }

  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "Greenhouse Gas Emissions",
                       type = "EDGAR Greenhouse Gas Emissions 2024",
                       filename = "EDGAR_2024_GHG_booklet_2024.XLSX",
                       `Indicative size (MB)` = 3.9,
                       dimensions = "2D",
                       unit = "Mt CO2-equiv/yr",
                       Confidential = "project"))
}
