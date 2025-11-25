#' convertIEA2024
#'
#' The ISO codes of "IEA2024" data are compared with the official ISO code country list.
#' NA values are replaced with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "IEA2024" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#' 
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA2024", convert = TRUE)
#' }
#'

convertIEA2024 <- function(x) {
  
  x <- as.quitte(x)
  
  suppressWarnings({
    levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]), mapping =
                                                   c("Bolivarian Republic of Venezuela" = "VEN",
                                                     "China (P.R. of China and Hong Kong, China)" = "CHA",
                                                     "Kingdom of Eswatini" = "SWZ",
                                                     "Republic of the Congo" = "COG",
                                                     "Republic of Turkiye" = "TUR",
                                                     "IEAFAMILY" = "GLO"))
  })

  x <- filter(x, !is.na(x[["region"]]))
  x <- as.magpie(x)
  suppressWarnings({
    x <- toolCountryFill(x, fill = NA)
  })
  return(x[as.character(getISOlist()), , ])
  
}
