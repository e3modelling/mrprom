#' readENGAGE
#'
#' Read in data from the ENGAGE project.
#' The dataset contains carbon price data for the current policies scenario GP_CurPol_T45.
#'
#'
#' @return The read-in carbon price data into a magpie object
#'
#' @author Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- readSource("ENGAGE")
#' }
#'
#' @importFrom dplyr filter
#' @importFrom utils read.csv
#'
readENGAGE <- function() {


  x <- read.csv(file = "engage-internal_snapshot_1700571205.csv")

  x <- filter(x, x[["Region"]] != "")
  names(x) <- sub("X", "", names(x))

  x[["Region"]] <- toolCountry2isocode(x[["Region"]], mapping = c("European Union (28 member countries)" = "EUR",
                                                                    "Republic of India" = "IND",
                                                                    "Federative Republic of Brazil" = "BRA",
                                                                    "Republic of Turkey" = "TUR",
                                                                    "Argentine Republic" = "ARG",
                                                                    "State of Japan" = "JPN",
                                                                    "Republic of Indonesia" = "IDN",
                                                                    "United Mexican States" = "MEX",
                                                                    "Republic of South Africa" = "ZAF",
                                                                    "Commonwealth of Australia" = "AUS",
                                                                    "Kingdom of Saudi Arabia" = "SAU",
                                                                    "Republic of Korea (South Korea)" = "KOR",
                                                                    "Thailand " = "THA",
                                                                    "Viet Nam " = "VNM",
                                                                    "People's Repulic of China" = "CHN"))
  x <- as.magpie(x)
  getSets(x)[2] <- "Year"
  return(x)

}
