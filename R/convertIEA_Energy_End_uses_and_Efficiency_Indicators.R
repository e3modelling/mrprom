#' convertIEA_Energy_End_uses_and_Efficiency_Indicators
#'
#' The ISO codes of "IEA_Energy_End_uses_and_Efficiency_Indicators" data are
#' compared with the official ISO code country list. NA values are replaced
#' with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "IEA_Energy_End_uses_and_Efficiency_Indicators" data with spatial
#' entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#' 
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_Energy_End_uses_and_Efficiency_Indicators", subtype = "GENERIC", convert = TRUE)
#' }
#'

convertIEA_Energy_End_uses_and_Efficiency_Indicators <- function(x) {
  
  x <- as.quitte(x)
  
  x[["region"]] <- toolCountry2isocode(x[["region"]], mapping =
                                         c("Bolivarian Republic of Venezuela" = "VEN",
                                           "China (P.R. of China and Hong Kong, China)" = "CHA",
                                           "Kingdom of Eswatini" = "SWZ",
                                           "Republic of the Congo" = "COG",
                                           "Republic of Turkiye" = "TUR",
                                           "IEAFAMILY" = "GLO"))
  x <- filter(x, !is.na(x[["region"]]))
  x <- as.quitte(x)
  x <-  as.magpie(x)
  suppressWarnings({
    x <- toolCountryFill(x, fill = NA)
  })
  return(x[as.character(getISOlist()), , ])
  
}
