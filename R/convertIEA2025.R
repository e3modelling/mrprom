#' convertIEA2025
#'
#' The ISO codes of "IEA2025" data are compared with the official ISO code country list.
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
#' a <- readSource("IEA2025", convert = TRUE)
#' }
#'
convertIEA2025 <- function(x) {
  x <- as.quitte(x)

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
  x <- filter(x, !is.na(x[["region"]]))
  x <- as.magpie(x)
  x <- toolCountryFill(x, fill = NA)
  return(x[as.character(getISOlist()), , ])
}
