#' toolFixWithRegional
#'
#' For the given MAgPIE object with ISO country codes, this function returns 
#' a MAgPIE object with the regional mean value in which each country is,
#' for the specific region, period and variable. 
#' If there is no data for the country the region value is taken.
#' If there is no data for the region the global value is taken.
#'
#' @param x MAgPIE object with ISO country codes.
#'
#' @param h12 type of region mapping.
#'
#' @return magpie object with the requested output data about regional 
#' mean value.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- toolFixWithRegional(x, h12)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr select %>% left_join mutate
#'
#' @export

toolFixWithRegional <- function(x, h12) {
  
  qx <- as.quitte(x)
  qx_bu <- as.quitte(x)
  names(qx) <- sub("region", "CountryCode", names(qx))
  ## add h12 mapping to dataset
  qx <- left_join(qx, h12, by="CountryCode")
  ## add new column containing regional mean value
  value <- NULL
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "new", "variable"))
  names(qx) <- sub("CountryCode", "region", names(qx))
  qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  qx_bu <- select(qx_bu, -c("model", "scenario"))
  ## assign to countries with NA, their H12 region mean
  value.x <- NULL
  value.y <- NULL
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "new", "unit")) %>% 
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>% 
    select(-c("value.x", "value.y"))
  ## assign to countries that still have NA, the global mean
  qx_bu <- qx
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "new", "variable"))
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "new", "unit")) %>% 
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>% 
    select(-c("value.x", "value.y"))
  x <- as.quitte(qx) %>% as.magpie()
  
  return(x)
}