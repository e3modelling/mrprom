#' convertICTPrisma
#'
#' The ISO codes data are compared with the official ISO code country list.
#' NA values are replaced with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "ICTPrisma" data with spatial entries for each country.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("ICTPrisma", convert = TRUE)
#' }
#'

convertICTPrisma <- function(x) {
  
  x <- as.quitte(x)
  
  suppressWarnings({
    levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]), mapping =
                                                   c("Ethopia" = "ETH"))
  })
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  suppressMessages(
    suppressWarnings(
      x <- toolCountryFill(x, fill = 0)
    )
  )
  return(x[as.character(getISOlist()), , ])
  
}
