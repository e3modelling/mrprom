#' convertIEA_PRICES
#'
#' The ISO codes of "IEA_PRICES" data are compared with the official ISO code country list.
#' NA values are replaced with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "IEA_PRICES" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#' 
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_PRICES", convert = TRUE)
#' }
#'

convertIEA_PRICES <- function(x) {
  
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
  suppressMessages(
    suppressWarnings(
      x <- toolCountryFill(x)
    )
  )
  
  return(x[as.character(getISOlist()), , ])
  
}
