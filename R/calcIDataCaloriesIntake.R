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
  # assign to countries with NA, their H12 region with weights
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  
  qx <- select(qx, -c("model", "scenario"))
  qx_bu <- qx
  
  ## assign to countries with NA, their H12 region with weights calculated from population
  
  population <- calcOutput(type = "POP", aggregate = FALSE)
  population <- as.quitte(population)
  
  # compute weights by population
  names(population) <- sub("region", "CountryCode", names(population))
  
  ## add mapping to population
  population <- left_join(population, h12, by = "CountryCode")
  value.x <- NULL
  value.y <- NULL
  weights <- NULL
  value <- NULL
  POP <- mutate(population, weights = sum(value, na.rm = TRUE), .by = c("RegionCode", "period"))
  POP["weights"] <- POP["value"] / POP["weights"]
  
  names(POP) <- sub("CountryCode", "region", names(POP))
  POP <- select(POP, -c("value", "model", "scenario", "X", "variable", "unit"))
  qx <- left_join(qx, POP, by = c("region", "period"))
  
  qx <- mutate(qx, value = sum(value, na.rm = TRUE), .by = c("RegionCode", "period", "item", "variable", "unit"))
  
  qx["value"] <- qx["value"] * qx["weights"]
  
  qx <- select(qx, -c("weights"))
  
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "item", "unit")) %>%
    mutate(value = ifelse(isZero(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y", "RegionCode"))
  
  ## assign to countries that still have NA, the global with weights
  qx_bu <- qx
  # compute weights by population
  POP <- mutate(population, weights = sum(value, na.rm = TRUE), .by = c("period"))
  POP["weights"] <- POP["value"] / POP["weights"]
  names(POP) <- sub("CountryCode", "region", names(POP))
  POP <- select(POP, -c("value", "model", "scenario", "X", "RegionCode", "variable", "unit"))
  qx <- left_join(qx, POP, by = c("region", "period"))
  
  qx <- mutate(qx, value = sum(value, na.rm = TRUE), .by = c("period", "item", "variable", "unit"))
  
  qx["value"] <- qx["value"] * qx["weights"]
  
  qx <- select(qx, -c("weights"))
  
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "item", "unit")) %>%
    mutate(value = ifelse(isZero(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  x <- as.quitte(qx) %>% as.magpie()
  
  x <- add_dimension(
    x,
    dim = 3.2,
    add = "unit",
    nm = "kcal/capita/day"
  )
  
  list(
    x = x,
    weight = NULL,
    unit = "kcal/capita/day",
    description = "Food supply (kcal/capita/day)"
  )
}
