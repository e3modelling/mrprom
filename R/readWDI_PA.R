#' readWDI_PA
#'
#' Read in a csv file and convert it to a magpie object
#' The data has information about air transport passengers per country and per year
#'
#' @return magpie object with the requested output data about air transport passengers per country and per year
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("WDI_PA")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#' @importFrom dplyr %>%
#' @importFrom dplyr select
#' @importFrom utils read.csv
#' @importFrom tidyr gather
#' @importFrom readxl read_excel
#'

readWDI_PA <- function() {

  x <- read_excel("API_IS.AIR.PSGR_DS2_en_excel_v2_1404.xls", sheet = "Data", skip = 3 )
  x <- x[,-c(1,3,4)]
  names(x) <- gsub("Country Code","region",names(x))
  
  suppressWarnings({
    levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]),
                                                       mapping = c("Bahamas, The" = "BHS",
                                                                   "Congo, Rep." = "COG",
                                                                   "Gambia, The" = "GMB",
                                                                   "Hong Kong SAR, China" = "HKG",
                                                                   "Korea, Dem. People's Rep." = "PRK",
                                                                   "Macao SAR, China" = "MAC",
                                                                   "China, Macao" = "MAC",
                                                                   "Egypt, Arab Rep." = "EGY",
                                                                   "Korea, Rep." = "KOR",
                                                                   "Micronesia, Fed. Sts." = "FSM",
                                                                   "St. Helena" = "SHN",
                                                                   "St. Kitts and Nevis" = "KNA",
                                                                   "St. Lucia" = "LCA",
                                                                   "St. Vincent and the Grenadines" = "VCT",
                                                                   "Venezuela, RB" = "VEN",
                                                                   "Yemen, Rep." = "YEM",
                                                                   "Congo, Dem. Rep." = "COD",
                                                                   "Iran, Islamic Rep." = "IRN",
                                                                   "Category" = "NA",
                                                                   "East Asia & Pacific (excluding high income)" = "NA",
                                                                   "East Asia & Pacific (IDA & IBRD countries)" = "NA",
                                                                   "Middle East & North Africa (excluding high income)" = "NA",
                                                                   "Fragile and conflict affected situations" = "NA",
                                                                   "Middle East & North Africa (IDA & IBRD countries)" = "NA",
                                                                   "Europe & Central Asia (IDA & IBRD countries)" = "NA",
                                                                   "Latin America & the Caribbean (IDA & IBRD countries)" = "NA",
                                                                   "Europe & Central Asia (excluding high income)" = "NA",
                                                                   "Latin America & Caribbean (excluding high income)" = "NA",
                                                                   "Sub-Saharan Africa (excluding high income)" = "NA",
                                                                   "Sub-Saharan Africa (IDA & IBRD countries)" = "NA",
                                                                   "Heavily indebted poor countries (HIPC)" = "NA",
                                                                   "Turkiye" = "TUR",
                                                                   "Central Europe and the Baltics" = "NA"))
  })

  x <- x %>% filter(x[["region"]] != "NA")
  
  x <-  x %>% 
    filter(region != "NA") %>% 
    pivot_longer(
      cols = -region,      # pivot all columns EXCEPT region
      names_to = "period",
      values_to = "value"
    )
  
  x[["unit"]] <- "passengers"
  x[["value"]] <- as.numeric(x[["value"]])
  x <- as.magpie(as.quitte(x))
  
  list(x = x,
       weight = NULL,
       description = c(category = "Transport",
                       type = "Air Transport Passengers",
                       filename = "API_IS.AIR.PSGR_DS2_en_csv_v2_5454874.csv",
                       `Indicative size (MB)` = 0.142,
                       dimensions = "2D",
                       unit = "varius",
                       Confidential = "open"))

}
