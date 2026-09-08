#' calcIDataCaloriesIntake
#'
#' @return  Magpie object with the Food supply (kcal/capita/day)
#'
#' @author Fotis Sioutas
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataCaloriesIntake", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter left_join mutate select %>%
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom R.utils isZero

calcIDataCaloriesIntake <- function() {

  x <- readSource("FAO", convert = TRUE)
  x <- collapseDim(x, 3.4)
  x <- collapseDim(x, 3.3)
  x <- collapseDim(x, 3.2)
  x <- collapseDim(x, 3.1)
  x[is.na(x)] <- 0
  
  FAO_item <- toolGetMapping(
    name = "FAO_item_mapping_no_double_count_v3.csv",
    type = "sectoral",
    where = "mrprom") %>%
    filter(category != "DROP")
  
  xAggr <- toolAggregate(x[,,FAO_item[["item"]]], dim=3, rel = FAO_item, from="item", to="category")
  
  # complete incomplete time series
  qx <- as.quitte(xAggr) %>%
    interpolate_missing_periods(period = getYears(xAggr, as.integer = TRUE), expand.values = TRUE)
 
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
  
  historical <- add_dimension(
    x,
    dim = 3.2,
    add = "unit",
    nm = "kcal/capita/day"
  )
  
  ########projections
  projections <- readSource("FAOprojections")
  projections <- projections[,,"Business As Usual"][,,"kcal/person/day"][,,"Daily energy supply"]
  projections <- collapseDim(projections, 3.3)
  projections <- collapseDim(projections, 3.2)
  projections <- collapseDim(projections, 3.1)
  projections <- projections[,c(2030,2035,2040,2050),]
  projections[is.na(projections)] <- 0
  
  FAO_item_projections <- toolGetMapping(
    name = "FAO_item_mapping_projections.csv",
    type = "sectoral",
    where = "mrprom") %>%
    filter(category != "DROP")
  
  xAggrprojections <- toolAggregate(projections[,,FAO_item_projections[["item"]]], dim=3, rel = FAO_item_projections, from="item", to="category")
  
  # complete incomplete time series
  qx <- as.quitte(xAggrprojections) %>%
    interpolate_missing_periods(period = getYears(xAggrprojections, as.integer = TRUE), expand.values = TRUE)
  
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
  
  projections <- add_dimension(
    x,
    dim = 3.2,
    add = "unit",
    nm = "kcal/capita/day"
  )
  
  data <- mbind(historical, projections)
  
  # complete incomplete time series
  qx <- as.quitte(data) %>%
    interpolate_missing_periods(period = 2010:2100, expand.values = TRUE)
  
  x <- as.magpie(qx)
  
  # Calculation of aggregation weights
  POP <- calcOutput("POP", aggregate = FALSE) # will use POP as disaggregation weights
  POP <- POP[, getYears(x), , drop = TRUE]
  weights <- x
  weights[, , ] <- POP
  
  list(
    x = x,
    weight = weights,
    unit = "kcal/capita/day",
    description = "Food supply (kcal/capita/day)",
    mixed_aggregation = TRUE
  )
}
