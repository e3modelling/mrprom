#' convertIEACrudeImportCountry
#'
#' The ISO codes of "IEACrudeImportCountry" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "IEACrudeImportCountry" data with spatial entries for each country.
#'
#' @author Fotis Sioutas
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEACrudeImportCountry", convert = TRUE)
#' }
#'
convertIEACrudeImportCountry <- function(x) {
  x <- as.quitte(x)
  
  suppressWarnings({
    levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]),
                                                 mapping =
                                                   c(
                                                     "WORLD" = "GLO",
                                                     "SOUTHAFRICA" = "ZAF",
                                                     "EU27" = "EU27",
                                                     "EU28" = "EU28",
                                                     "OECDAM" = "OECDAM",
                                                     "OECDAO" = "OECDAO",
                                                     "OECDEUR" = "OECDEUR",
                                                     "OECDTOT" = "OECDTOT",
                                                     "NEWZEALAND" = "NZL",
                                                     "BURKINAFASO" = "BFA",
                                                     "CONGO_DRC" = "COD",
                                                     "CONGO_REPUB" = "COG",
                                                     "DOMINICANREP" = "DOM",
                                                     "SAUDIARABIA" = "SAU"
                                                   )
    )
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
