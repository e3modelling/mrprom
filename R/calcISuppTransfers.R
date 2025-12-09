#' calcISuppTransfers
#'
#' Use data from IEA to derive OPENPROM input parameter iSuppTransfers
#' This dataset includes the supplementary parameter for transfers, in Mtoe.
#'
#' @return magpie object with OPENPROM input data iSuppTransfers.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "ISuppTransfers", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join case_when if_else arrange
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom tibble deframe
#' @importFrom utils tail

calcISuppTransfers <- function() {

  fuelMap <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, variable = OPEN.PROM)
  
  # load TRANSFERS data from IEA
  x <- readSource("IEA2025", subset = "TRANSFERS") %>%
    as.quitte() %>%
    filter(unit == "KTOE", product != "TOTAL", !is.na(value)) %>%
    select(-variable) %>%
    mutate(unit = "Mtoe", value = value / 1000) %>%
    inner_join(fuelMap, by = "product") %>%
    group_by(region, period, variable) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    as.quitte() %>%
    as.magpie() %>%
    # FIXME: Proper impute must be done. For now fill with zero.
    toolCountryFill(fill = NA)
  
  # complete incomplete time series
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = getYears(x, as.integer = TRUE), expand.values = TRUE)
  # assign to countries with NA, their H12 region with weights
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  
  qx <- select(qx, -c("model", "scenario"))
  qx_bu <- qx
  
  ## assign to countries with NA, their H12 region with weights calculated from population and GDP
  # start with population
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
  # Compute POP Weights
  POP <- mutate(population, weights = sum(value, na.rm = TRUE), .by = c("RegionCode", "period"))
  POP["weights"] <- POP["value"] / POP["weights"]
  
  names(POP) <- sub("CountryCode", "region", names(POP))
  POP <- select(POP, -c("value", "model", "scenario", "X", "variable", "unit"))
  
  # Load GDP
  gdp <- calcOutput(type = "iGDP", aggregate = FALSE)
  gdp <- as.quitte(gdp)
  
  ## add mapping to gdp
  names(gdp) <- sub("region", "CountryCode", names(gdp))
  gdp <- left_join(gdp, h12, by = "CountryCode")
  
  # Compute GDP Weights
  GDP <- mutate(gdp, weights_gdp = sum(value, na.rm = TRUE), 
                .by = c("RegionCode", "period"))
  
  GDP["weights_gdp"] <- GDP["value"] / GDP["weights_gdp"]
  
  names(GDP) <- sub("CountryCode", "region", names(GDP))
  
  GDP <- select(GDP, -c("value", "model", "scenario", "X", "variable", "unit"))
  
  # Merge POP + GDP and Create Mixed Weights
  POP_GDP <- left_join(POP, GDP, by = c("region", "period", "RegionCode"))
  
  alpha <- 0.5   # ← change this to control POP vs GDP importance
  
  POP_GDP <- mutate(
    POP_GDP,
    weights = alpha * weights + (1 - alpha) * weights_gdp
  )
  
  POP_GDP <- select(POP_GDP, -c(weights_gdp))
  
  ############
  qx <- left_join(qx, POP_GDP, by = c("region", "period"))
  
  qx <- mutate(qx, value = sum(value, na.rm = TRUE), .by = c("RegionCode", "period", "variable", "unit"))
  
  qx["value"] <- qx["value"] * qx["weights"]
  
  qx <- select(qx, -c("weights"))
  
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y", "RegionCode"))
  
  ## assign to countries that still have NA, the global with weights calculated from population and GDP
  qx_bu <- qx
  # compute weights by population
  POP <- mutate(population, weights = sum(value, na.rm = TRUE), .by = c("period"))
  POP["weights"] <- POP["value"] / POP["weights"]
  names(POP) <- sub("CountryCode", "region", names(POP))
  POP <- select(POP, -c("value", "model", "scenario", "X", "RegionCode", "variable", "unit"))
  
  # Compute GDP Weights
  GDP <- mutate(gdp, weights_gdp = sum(value, na.rm = TRUE), 
                .by = c("period"))
  
  GDP["weights_gdp"] <- GDP["value"] / GDP["weights_gdp"]
  
  names(GDP) <- sub("CountryCode", "region", names(GDP))
  
  GDP <- select(GDP, -c("value", "model", "scenario", "X", "variable", "unit", "RegionCode"))
  
  # Merge POP + GDP and Create Mixed Weights
  POP_GDP <- left_join(POP, GDP, by = c("region", "period"))
  
  alpha <- 0.5   # ← change this to control POP vs GDP importance
  
  POP_GDP <- mutate(
    POP_GDP,
    weights = alpha * weights + (1 - alpha) * weights_gdp
  )
  
  POP_GDP <- select(POP_GDP, -c(weights_gdp))
  
  qx <- left_join(qx, POP_GDP, by = c("region", "period"))
  
  qx <- mutate(qx, value = sum(value, na.rm = TRUE), .by = c("period", "variable", "unit"))
  
  qx["value"] <- qx["value"] * qx["weights"]
  
  qx <- select(qx, -c("weights"))
  
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  
  # Converting to magpie object
  x <- as.quitte(qx) %>% as.magpie()
  
  # Set NA to 0
  x[is.na(x)] <- 0
  
  list(x = x,
       weight = NULL,
       unit = "Mtoe",
       description = "IEA; Supplementary parameter for transfers")
}
