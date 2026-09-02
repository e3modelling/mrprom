#' readEDGAR2026
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("EDGAR2026", convert = TRUE)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter select
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' 
readEDGAR2026 <- function(subtype = "IPCC 2006") {
  
  if (subtype == "IPCC 2006") {
    x <- read_excel("EDGAR_AR5_GHG_1970_2024.xlsx",
                  sheet = subtype, skip = 8)
    
    x <- select(x, -c("IPCC_annex","C_group_IM24_sh","Name","ipcc_code_2006_for_standard_report"))
    
    names(x)[names(x) == "Country_code_A3"] <- "region"
    names(x)[names(x) == "ipcc_code_2006_for_standard_report_name"] <- "variable"
    
    x <- x %>%
      pivot_longer(
        cols = starts_with("Y_"),
        names_to = "period",
        values_to = "value"
      ) %>%
      mutate(
        period = as.numeric(sub("Y_", "", period))
      )
    
    x <- as.quitte(x)
    
    x$region <- as.character(x$region)
    
    levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]), mapping =
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
    
  
    x[["unit"]] <-  "Mt CO2-equiv/yr"
    x[["value"]] <-  x[["value"]] / 1000
    
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "TOTALS BY COUNTRY") {
    x <- read_excel("EDGAR_AR5_GHG_1970_2024.xlsx",
                    sheet = subtype, skip = 8)
    
    x <- select(x, -c("IPCC_annex","C_group_IM24_sh","Name"))
    
    names(x)[names(x) == "Country_code_A3"] <- "region"
    
    x <- x %>%
      pivot_longer(
        cols = starts_with("Y_"),
        names_to = "period",
        values_to = "value"
      ) %>%
      mutate(
        period = as.numeric(sub("Y_", "", period))
      )
    
    x <- as.quitte(x)
    
    x$region <- as.character(x$region)
    
    levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]), mapping =
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
    
    
    x[["unit"]] <-  "Mt CO2-equiv/yr"
    x[["value"]] <-  x[["value"]] / 1000
    
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "IPCC 2006 CO2") {
    x <- read_excel("IEA_EDGAR_CO2_1970_2024.xlsx",
                    sheet = "IPCC 2006", skip = 8)
    
    x <- select(x, -c("IPCC_annex","C_group_IM24_sh","Name","ipcc_code_2006_for_standard_report"))
    
    names(x)[names(x) == "Country_code_A3"] <- "region"
    names(x)[names(x) == "ipcc_code_2006_for_standard_report_name"] <- "variable"
    
    x <- x %>%
      pivot_longer(
        cols = starts_with("Y_"),
        names_to = "period",
        values_to = "value"
      ) %>%
      mutate(
        period = as.numeric(sub("Y_", "", period))
      )
    
    x <- as.quitte(x)
    
    x$region <- as.character(x$region)
    
    levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]), mapping =
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
    
    
    x[["unit"]] <-  "Mt CO2-equiv/yr"
    x[["value"]] <-  x[["value"]] / 1000
    
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "TOTALS BY COUNTRY CO2") {
    x <- read_excel("IEA_EDGAR_CO2_1970_2024.xlsx",
                    sheet = "TOTALS BY COUNTRY", skip = 8)
    
    x <- select(x, -c("IPCC_annex","C_group_IM24_sh","Name"))
    
    names(x)[names(x) == "Country_code_A3"] <- "region"
    
    x <- x %>%
      pivot_longer(
        cols = starts_with("Y_"),
        names_to = "period",
        values_to = "value"
      ) %>%
      mutate(
        period = as.numeric(sub("Y_", "", period))
      )
    
    x <- as.quitte(x)
    
    x$region <- as.character(x$region)
    
    levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]), mapping =
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
    
    
    x[["unit"]] <-  "Mt CO2/yr"
    x[["value"]] <-  x[["value"]] / 1000
    
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  list(x = x,
       weight = NULL,
       description = c(category = "Greenhouse Gas Emissions",
                       type = "EDGAR Greenhouse Gas Emissions 2024",
                       filename = "EDGAR_AR5_GHG_1970_2024.xlsx",
                       `Indicative size (MB)` = 3.9,
                       dimensions = "2D",
                       unit = "Mt CO2-equiv/yr",
                       Confidential = "project"))
}
