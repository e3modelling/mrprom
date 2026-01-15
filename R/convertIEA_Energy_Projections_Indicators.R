#' convertIEA_Energy_Projections_Indicators
#'
#' The ISO codes of "IEA_Energy_Projections_Indicators" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "IEA_Energy_Projections_Indicators" data with spatial entries for each country.
#'
#' @author Fotis Sioutas
#' 
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_Energy_Projections_Indicators", convert = TRUE)
#' }
#'

convertIEA_Energy_Projections_Indicators <- function(x) {
  
  x <- as.quitte(x)
  
  suppressWarnings({
    levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]), mapping =
                                                   c("Bolivarian Republic of Venezuela" = "VEN",
                                                     "China (P.R. of China and Hong Kong, China)" = "CHA",
                                                     "Kingdom of Eswatini" = "SWZ",
                                                     "Republic of the Congo" = "COG",
                                                     "Republic of Turkiye" = "TUR",
                                                     "IEAFAMILY" = "GLO",
                                                     "EU27" = "EU27"))
  })
  
  x <- filter(x, !is.na(x[["region"]]))
  
  x <- as.magpie(x)
  suppressMessages(
    suppressWarnings(
      x <- toolCountryFill(x, fill = NA)
    )
  )
  
  return(x[as.character(getISOlist()), , ])
  
}
