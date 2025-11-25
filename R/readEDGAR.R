#' readEDGAR
#'
#' Read IEA EDGAR CO2 emissions from EDGAR.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- readSource("EDGAR", convert = TRUE)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readEDGAR <- function() {
  
  x <- read_excel("IEA_EDGAR_CO2_1970_2022.xlsx",
                  sheet = "TOTALS BY COUNTRY", range = "C10:BF233")
  
  x <- x %>% pivot_longer(!c("Country_code_A3", "Name", "Substance" ), names_to = "period", values_to = "value")
 
  x <- as.data.frame(x)
  x["period"] <- gsub("Y_", "", as.factor(x[["period"]]))
  x <- x[, c(2, 4, 5)]
  names(x)[1] <- "region"
  
  suppressWarnings({
    x[["region"]] <- toolCountry2isocode((x[["region"]]), mapping =
                                           c("Congo_the Democratic Republic of the" = "COD",
                                             "Tanzania_United Republic of" = "TZA",
                                             "Virgin Islands_British" = "VGB",
                                             "Virgin Islands_USA" = "VIR",
                                             "Taiwan_Province of China" = "TWN"))
  })
  
  x <- filter(x, !is.na(x[["region"]]))
  x <- as.quitte(x)
  x["unit"] <- "MtCO2"
  x <- as.magpie(x)
  x <- x / 1000
  
  list(x = x,
       weight = NULL,
       description = c(category = "Greenhouse Gas Emissions",
                       type = "EDGAR Greenhouse Gas Emissions",
                       filename = "IEA_EDGAR_CO2_1970_2022.xls",
                       `Indicative size (MB)` = 5,
                       dimensions = "2D",
                       unit = "MtCO2",
                       Confidential = "project"))
}
