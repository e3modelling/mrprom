#' convertGlobalSolarAtlas
#'
#' The ISO codes of "GlobalSolarAtlas" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "GlobalSolarAtlas" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("GlobalSolarAtlas", convert = TRUE)
#' }
#'
#' @importFrom dplyr filter select %>% mutate
#' @importFrom quitte as.quitte

convertGlobalSolarAtlas <- function(x)
{ x <- as.quitte(x)
  x <- filter(x, !is.na(x[["region"]]))
  x <- as.quitte(x) %>% as.magpie()
  x <- toolCountryFill(x, fill = NA) 
  return(x[as.character(getISOlist()), , ])
  
}
