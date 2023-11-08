#' readEurostat_ELVS
#'
#' Read in a csv file and convert it to a magpie object
#' The data has information about End-of-life vehicles.
#'
#' @return magpie object with the requested output data about
#' End-of-life vehicles.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("Eurostat_ELVS")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom stringr str_remove
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% across mutate
#' @importFrom readxl read_excel
#' @importFrom stringi stri_escape_unicode
#'

readEurostat_ELVS <- function() {

  scrap <- read_excel("SE_End-of-life_vehicle_statistics_2023-07.xlsx",
                      sheet = "Table 1", range = "B4:O35")

  percentage_reuse <- read_excel("SE_End-of-life_vehicle_statistics_2023-07.xlsx",
                                 sheet = "Table 2", range = "B4:O35")

  scrap <- scrap %>%
    mutate(across(c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015",
                    "2016", "2017", "2018", "2019", "2020"),
                  ~ as.numeric(str_remove(., "[A-Za-z]"))))

  percentage_reuse <- percentage_reuse %>%
    mutate(across(c("2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015",
                    "2016", "2017", "2018", "2019", "2020"),
                  ~ as.numeric(str_remove(., "[A-Za-z]"))))

  reuse_rate <- scrap[, 2:length(scrap)] * (percentage_reuse[, 2:length(percentage_reuse)] / 100)

  reuse_rate <- cbind(scrap[, 1], reuse_rate)

  reuse_rate <- reuse_rate[-1, ]
  names(reuse_rate)[1] <- "region"

  reuse_rate[, "region"] <- stri_escape_unicode(reuse_rate[, "region"])

  reuse_rate[, "region"] <- toolCountry2isocode((reuse_rate[, "region"]), mapping = c("Croatia (\\u00b2)" = "HRV",
                                                                  "Malta (\\u00b3)" = "MLT",
                                                                  "Iceland (\\u00b3)" = "ISL"))

  x <- as.quitte(reuse_rate) %>% as.magpie()

  return(x)

}
