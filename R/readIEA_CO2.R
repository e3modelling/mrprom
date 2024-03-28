#' readIEA_CO2
#'
#' Read IEA_CO2 2021 emissions from IEA.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_CO2", convert = TRUE)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readIEA_CO2 <- function() {
  
  x <- read_excel("GHGHighlights.xls",
                  sheet = "SECTOR", range = "A22:I196")
  
  x <- x %>% pivot_longer(!("Region/Country/Economy"), names_to = "variable", values_to = "value")
  names(x)[1] <- "region"
  x["value"] <- as.numeric(x[["value"]])
  x <- as.quitte(x)
  x <- filter(x, !is.na(x[["value"]]))
  x["period"] <- "2021"
  x <- filter(x, !is.na(x[["region"]]))
  x["unit"] <- "MtCO2"
  x[, 3] <- as.character(x[, 3])
  x[305:312, 3] <- iconv(x[305, 3], from = "UTF-8", to = "ASCII//TRANSLIT")
  
  x[["region"]] <- toolCountry2isocode((x[["region"]]), mapping =
                                         c("Dem. Rep. of Congo" = "COD",
                                           "DPR of Korea" = "PRK",
                                           "Islamic Rep. of Iran" = "IRN",
                                           "Kingdom of Eswatini" = "SWZ",
                                           "People's Rep. of China" = "CHN",
                                           "Republic of Turkiye" = "TUR",
                                           "United Rep. of Tanzaniae" = "TZA"))
  x <- as.quitte(x)
  x <- filter(x, !is.na(x[["region"]]))
  x <- as.magpie(x)

  return(x)
}
