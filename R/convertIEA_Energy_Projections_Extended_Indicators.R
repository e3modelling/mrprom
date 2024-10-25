#' convertIEA_Energy_Projections_Extended_Indicators
#'
#' The ISO codes of "IEA_Energy_Projections_Extended_Indicators" data are compared with the official ISO code country list.
#' NA values are replaced with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "IEA_Energy_Projections_Extended_Indicators" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#' 
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_Energy_Projections_Extended_Indicators", convert = TRUE)
#' }
#'

convertIEA_Energy_Projections_Extended_Indicators <- function(x) {
  
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
  x <- toolCountryFill(x)
  x <- collapseDim(x, dim = c(3.2))
  
  return(x[as.character(getISOlist()), , ])
  
}
