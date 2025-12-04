#' convertWorldBankCarPr
#'
#' Add missing periods to data frame and interpolate missing values.
#' The ISO codes of "WorldBankCarPr" data are compared with the official ISO code country list.
#' 
#' @param x MAgPIE object.
#'
#' @return The "IRF" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("WorldBankCarPr", convert = TRUE)
#' }
#'

convertWorldBankCarPr <- function(x) {
  
  x <- toolCountryFill(x, fill = NA)
  return(x[as.character(getISOlist()), , ])
  
}
