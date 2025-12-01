#' readLandAreaCountries
#'
#' Read in a csv file and convert it to a magpie object
#' The data has information about LandAreaCountries in sq per km2.
#'
#' @return magpie object with the requested output data about
#' LandAreaCountries.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("LandAreaCountries")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>%
#' @importFrom utils read.csv
#'

readLandAreaCountries <- function() {
  
  x <- read.csv("countries_sq_per_km2.csv")
  names(x) <- c("region", "value")
  x[["unit"]] <- "sqkm"
  x[["period"]] <- 2025
  
  suppressWarnings({
    x[["region"]] <- toolCountry2isocode((x[["region"]]), mapping =
                                           c("Congo (Democratic Republic of the)" = "COD",
                                             "Congo (Congo-Brazzaville)" = "COG",
                                             "Myanmar (Burma)" = "MMR",
                                             " South Korea" = "KOR",
                                             " North Korea" = "PRK"))
  })
  
  x <- as.quitte(x) %>% as.magpie()
  suppressMessages(
    suppressWarnings(
      x <- toolCountryFill(x, fill = NA)
    )
  )
  
  list(x = x,
       weight = NULL,
       description = c(category = "Land",
                       type = "Land in sqkm of Countries",
                       filename = "countries_sq_per_km2.csv",
                       `Indicative size (MB)` = 0.004,
                       dimensions = "2D",
                       unit = "sqkm",
                       Confidential = "open"))
  
}