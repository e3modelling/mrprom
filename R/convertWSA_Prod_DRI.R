#' convertWSA_Prod_DRI
#'
#' The ISO codes of "WSA_Prod_DRI" data are compared with the official ISO code country list.
#' NA values are replaced with zeros
#'
#' @param x MAgPIE object.
#'
#' @return The "WSA_Prod_DRI" data with spatial entries for each country.
#'
#' @author Margarita Efthymiadou
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("WSA_Prod_DRI", convert = TRUE)
#' }
#'
convertWSA_Prod_DRI <- function(x) {
  
  x <- as.quitte(x)

  suppressWarnings({
    levels(x[["region"]]) <-toolCountry2isocode(levels(x[["region"]]), mapping =
                                                  c("Taiwan, China" = "TWN"))
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
