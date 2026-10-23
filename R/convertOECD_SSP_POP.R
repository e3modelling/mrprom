#' convertOECD_SSP_POP
#'
#' @param x MAgPIE object.
#'
#' @return The "OECD_SSP_GDP" data with spatial entries for each country.
#'
#' @author Fotis Sioutas
#' 
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("OECD_SSP_POP", convert = TRUE)
#' }
#'

convertOECD_SSP_POP <- function(x) {
  
  x <- as.quitte(x)
  
  suppressWarnings({
    levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]), mapping =
                                                   c("World" = "GLO"))
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
