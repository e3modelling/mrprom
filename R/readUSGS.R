#' readUSGS
#'
#' Read USGS RAW STEEL: WORLD PRODUCTION BY COUNTRY OR LOCALITY1, 2
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- readSource("USGS", convert = TRUE)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readUSGS <- function() {
  
  x <- read_excel("Steel_Production_USGS_myb1-2023-feste-ert.xlsx",
                  sheet = "T10")
  
  names(x) <- x[5,]
  x <- x[6:96,c(1,3,5,7,9,11)]
  
  x <- x %>%
    mutate(across(-`Country or locality3`, as.character))
  
  x <- x %>% pivot_longer(!("Country or locality3"), names_to = "period", values_to = "value")
  
  names(x)[1] <- "region"
  x["value"] <- as.numeric(x[["value"]])
  x <- as.quitte(x)
  x <- filter(x, !is.na(x[["value"]]))
  x <- filter(x, !is.na(x[["region"]]))
  x["unit"] <- "Mt"
  x["variable"] <- "IS"
  
  x[["region"]] <- toolCountry2isocode((x[["region"]]), mapping =
                                         c("Bangladeshe" = "BGD",
                                           "Burmae" = "MMR",
                                           "Jordane" = "JOR",
                                           "Kenyae" = "KEN",
                                           "Korea, North" = "PRK",
                                           "Montenegroe" = "MNE",
                                           "Nigeriae" = "NGA",
                                           "Omane" = "OMN",
                                           "Rwandae" = "RWA",
                                           "Tanzaniae" = "TZA",
                                           "Zambiae" = "ZMB",
                                           "Mauritaniae" = "MRT"))
  x <- as.quitte(x)
  x <- filter(x, !is.na(x[["region"]]))
  x <- as.magpie(x)
  
  x <- x / 1000#fix units
  
  list(x = x,
       weight = NULL,
       description = c(category = "USGS RAW STEEL: WORLD PRODUCTION BY COUNTRY OR LOCALITY1, 2",
                       type = "USGS RAW STEEL: WORLD PRODUCTION BY COUNTRY OR LOCALITY1, 2",
                       filename = "Steel_Production_USGS_myb1-2023-feste-ert.xlsx",
                       `Indicative size (MB)` = 0.95,
                       dimensions = "2D",
                       unit = "Mt",
                       Confidential = "E3M"))
}
