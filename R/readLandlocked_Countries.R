#' readLandlocked_Countries
#'
#' Read in a csv file and convert it to a magpie object
#' The data has information about Landlocked Countries.
#'
#' @return magpie object with the requested output data about
#' Landlocked Countries.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("Landlocked_Countries")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>%
#' @importFrom utils read.csv
#'

readLandlocked_Countries <- function() {
  
  x <- read.csv("countries.csv")
  
  suppressWarnings({
    x[["region"]] <- toolCountry2isocode(x[["region"]])
  })
  
  x <- as.quitte(x) %>% as.magpie()
  
  list(x = x,
       weight = NULL,
       description = c(category = "Land locked Countries",
                       type = "Land locked Countries",
                       filename = "countries.csv",
                       `Indicative size (MB)` = 0.004,
                       dimensions = "2D",
                       unit = "no",
                       Confidential = "open"))
  
}
