#' readBoT
#'
#' Read in a csv file and convert it to a magpie object
#' The data has information about End-of-life vehicles of USA.
#'
#' @return magpie object with the requested output data about
#' End-of-life vehicles of USA.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("BoT")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>%
#' @importFrom readxl read_excel
#'

readBoT <- function() {

  scrap <- read_excel("table_04_58q416.xlsx",
                      sheet = "4-58", range = "B2:AD3")

  scrap <- scrap %>% pivot_longer(cols = as.character(c(seq(from = 1970, to = 1990, by = 5),
                                                        seq(from = 1991, to = 2014, by = 1))),
                                  names_to = "period",
                                  values_to = "value")

  scrap["region"] <- "USA"

  y <- as.quitte(scrap) %>% as.magpie()
  x <- y * 1000

  list(x = x,
       weight = NULL,
       description = c(category = "Transport",
                       type = "End-of-life vehicles",
                       filename = "table_04_58q416.xlsx",
                       `Indicative size (MB)` = 0.018,
                       dimensions = "2D",
                       unit = "cars",
                       Confidential = "open"))
  

}
