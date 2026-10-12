#' calcIDataIntensityFertiliser
#'
#' @return  Magpie object with the Food supply (kg/ha)
#'
#' @author Fotis Sioutas
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataIntensityFertiliser", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter left_join mutate select %>%
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom R.utils isZero

calcIDataIntensityFertiliser <- function() {
  
  x <- readSource("FAOFertilizers", convert = TRUE)
  x <- x[,,"Use per area of cropland"]
  x <- collapseDim(x, 3.2)
  x <- collapseDim(x, 3.1)
  
  df <- data.frame(
    variable = c(
      "Nutrient nitrogen N (total)",
      "Nutrient phosphate P2O5 (total)",
      "Nutrient potash K2O (total)"
    ),
    nutrient = c("N", "P", "K")
  )
  
  x <- toolAggregate(x, dim=3, rel = df, from="variable", to="nutrient")
  x[is.na(x)] <- 0
  
  # complete incomplete time series
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = getYears(x, as.integer = TRUE), expand.values = TRUE)
  
  qx_bu <- qx
  
  # Assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  names(qx) <- sub("region", "CountryCode", names(qx))
  
  ## Add h12 mapping to dataset
  qx <- left_join(qx, h12, by = "CountryCode")
  
  ## Add new column containing regional mean value
  value <- NULL
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "item"))
  names(qx) <- sub("CountryCode", "region", names(qx))
  qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  qx_bu <- select(qx_bu, -c("model", "scenario"))
  
  ## Assign the H12 region mean where necessary
  value.x <- NULL
  value.y <- NULL
  qx <- left_join(qx_bu, qx, by = c("region", "item", "period", "unit")) %>%
    mutate(value = ifelse(isZero(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  
  ## Assign to countries that still have NA, the global mean
  qx_bu <- qx
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "item"))
  qx <- left_join(qx_bu, qx, by = c("region", "item", "period", "unit")) %>%
    mutate(value = ifelse(isZero(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  
  # Converting to magpie object
  x <- as.quitte(qx) %>% as.magpie()
  # Set NA to 0
  x[is.na(x)] <- 0
  
  x <- add_dimension(
    x,
    dim = 3.2,
    add = "unit",
    nm = "kg/ha"
  )
  
  qx <- as.quitte(x)
  x <- as.magpie(qx)
  
  # Calculation of aggregation weights
  weights <- calcOutput(type = "IDataAgricultureService", aggregate = FALSE)
  weights <- weights[,, "CROPS.1e9 ha"]
  
  list(
    x = x,
    weight = weights,
    unit = "kg/ha",
    description = "Intensity Fertiliser (kg/ha)",
    mixed_aggregation = TRUE
  )
}
