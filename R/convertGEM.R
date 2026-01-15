#' convertGEM
#'
#' The ISO codes of "GEM" data are compared with the official ISO
#' code country list.
#'
#' @param x Quitte object with ISO country codes.
#'
#' @return The "GEM" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("GEM", convert = TRUE)
#' }
#'
#' @importFrom dplyr %>% filter
#' @importFrom tidyr expand nesting
#'


convertGEM <- function(x) {

  a <- toolGetMissingCountries(unique(x[["region"]]))
  a <- as.data.frame(a)
  names(a) <- sub("a", "region", names(a))
  suppressWarnings({
    a[, "region"] <- toolCountry2isocode((a[, "region"]))
  })
  v <- a[, "region"]
  model <- NULL
  scenario <- NULL
  variable <- NULL
  unit <- NULL
  period <- NULL
  region <- NULL
  z <- expand(x, nesting(model, scenario, variable, unit, period), region = v)
  z["retired year"] <- NA
  z["plant name"] <- NA
  z["unit name"] <- NA
  z["value"] <- NA
  x <- rbind(x, z)
  x <- x %>% filter(region %in% as.character(getISOlist()))

  list(x = x,
       class = "quitte")
}
