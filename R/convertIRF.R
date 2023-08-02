#' convertIRF
#'
#' Add missing periods to data frame and interpolate missing values.
#' The ISO codes of "IRF" data are compared with the official ISO code country list.
#' NA values are replaced with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "IRF" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IRF", convert = TRUE)
#' }
#'
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr "%>%"

convertIRF <- function(x) {

  x <- as.quitte(x) %>%
    interpolate_missing_periods(period = seq(2015, 2020, 1),
                                expand.values = TRUE) %>% as.magpie()
  x <- toolCountryFill(x, fill = 0) #nolint
  return(x[as.character(getISOlist()), , ]) #nolint

}
