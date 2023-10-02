#' calcINewReg
#'
#' Calculate the passenger-car-first-registrations per year
#'
#' @return  Magpie object with the passenger-car-first-registrations per year
#' 
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "INewReg", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select left_join
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte interpolate_missing_periods


calcINewReg <- function() {
  
  x <- readSource("IRF", "passenger-car-first-registrations", convert = TRUE)
  #vehicles
  getNames(x) <- "passenger-car-first-registrations.million vehicles"
  x <- x/10^6
  #million vehicles
  
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = getYears(x, as.integer = TRUE), expand.values = TRUE)  
  qx_bu <- qx
  # assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv")
  names(qx) <- sub("region", "CountryCode", names(qx))
  ## add h12 mapping to dataset
  qx <- left_join(qx, h12, by="CountryCode")
  ## add new column containing regional mean value
  value <- NULL
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "unit", "variable"))
  names(qx) <- sub("CountryCode", "region", names(qx))
  qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  qx_bu <- select(qx_bu, -c("model", "scenario"))
  ## assign to countries with NA, their H12 region mean
  value.x <- NULL
  value.y <- NULL
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>% 
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>% 
    select(-c("value.x", "value.y"))
  ## assign to countries that still have NA, the global mean
  qx_bu <- qx
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "variable", "unit"))
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>% 
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>% 
    select(-c("value.x", "value.y"))
  
  x <- pivot_wider(qx, names_from = "period",values_from = "value") 
  
  x <- as.quitte(x) %>% as.magpie()
  
  list(x = x,
       weight = NULL,
       unit = "million vehicles",
       description = "IRF; passenger-car-first-registrations")
  
}