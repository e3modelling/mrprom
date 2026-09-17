#' calcIHeatpumpMix
#' 
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IHeatpumpMix", aggregate = TRUE)
#' }
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr filter select

calcIHeatpumpMix <- function() {
  
  EurostatHP <- readSource("EurostatHP", convert = TRUE)
  
  x <- EurostatHP
  # complete incomplete time series
  x <- as.quitte(x) %>%
    interpolate_missing_periods(period = 2010 : 2024, expand.values = TRUE) %>%
    as.magpie()
  
  x <- as.quitte(x) %>%
    group_by(variable, unit, period) %>%
    mutate(
      global_mean = mean(value, na.rm = TRUE),
      value = ifelse(is.na(value), global_mean, value)
    ) %>%
    select(-global_mean) %>%
    ungroup()
  
  x <- as.quitte(x) %>% as.magpie()
  
  total <- dimSums(x, dim = 3)
  
  x_share <- x / total
  
  x_share[is.na(x_share) | is.infinite(x_share)] <- 1/3
  
  weights <- calcOutput("iGDP", aggregate = FALSE)
  weights <- weights[,getYears(x_share),]
  
  list(x = x_share,
       weight = weights,
       unit = "%",
       description = "HeatpumpMix data")
}
