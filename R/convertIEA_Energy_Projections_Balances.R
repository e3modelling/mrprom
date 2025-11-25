#' convertIEA_Energy_Projections_Balances
#'
#' The ISO codes of "IEA_Energy_Projections_Balances" data are compared with the official ISO code country list.
#' NA values are replaced with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "IEA_Energy_Projections_Balances" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#' 
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_Energy_Projections_Balances", convert = TRUE)
#' }
#'

convertIEA_Energy_Projections_Balances <- function(x) {
  
  x <- as.quitte(x)
  
  levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]), mapping =
                                                 c("Bolivarian Republic of Venezuela" = "VEN",
                                                   "China (P.R. of China and Hong Kong, China)" = "CHA",
                                                   "Kingdom of Eswatini" = "SWZ",
                                                   "Republic of the Congo" = "COG",
                                                   "Republic of Turkiye" = "TUR",
                                                   "IEAFAMILY" = "GLO"))
  x <- filter(x, !is.na(x[["region"]]))
  
  x <- as.magpie(x)
  suppressWarnings({
    x <- toolCountryFill(x, fill = NA)
  })
  x <- collapseDim(x, dim = c(3.1, 3.3))
  
  return(x[as.character(getISOlist()), , ])
  
}
