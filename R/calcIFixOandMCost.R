#' calcIFixOandMCost
#'
#' Use data from IEA EU Reference Scenario to derive OPENPROM input parameter iFixOandMCost
#' This dataset includes Fixed O&M costs per plant type, in $2015/kW.
#'
#' @return magpie object with OPENPROM input data iFixOandMCost.
#'
#' @author Anastasis Giannousakis, Giannis Tolios, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IFixOandMCost", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select filter rename
#' @importFrom tidyr pivot_wider spread gather complete expand
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIFixOandMCost <- function() {

  x <- readSource("TechCosts2024", "PowerAndHeat", convert = TRUE)

  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fEndHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndHorizon"]

  # Use PRIMES - OPENPROM mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-primes-pgall-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")
  xq <- as.quitte(x)
  merged <- merge(map, xq, by.x = "PRIMES", by.y = "technology") # INNER JOIN

  variable <- NULL
  # Filtering the variable and dropping unnecessary columns
  xq <- filter(merged, variable == "Fixed Operation and Maintenance costs, annually")
  xq <- xq %>% select(-c("PRIMES", "variable")) %>%
    rename("variable" = "OPEN.PROM")

  # FIXME: Some power plant types are missing from EU Reference Scenario 2020
  #from:https://www.eia.gov/analysis/studies/powerplants/capitalcost/pdf/capital_cost_AEO2020.pdf
  df_missing <- data.frame(
    variable = c("ATHOIL", "ATHOIL", "ATHOIL", "ATHOIL"),
    model = rep("(Missing)", 4),
    scenario = rep("(Missing)", 4),
    region = rep("GLO", 4),
    unit = rep("$2015/kW", 4),
    period = c(2020, 2030, 2040, 2050),
    value = c(29.88, 29.88, 29.88, 29.88))
  xq <- rbind(xq, df_missing)

  # Interpolating the missing values for the specified time period
  xq <- interpolate_missing_periods(xq, seq(fStartHorizon, fEndHorizon, 1), expand.values = TRUE)

  # Converting to magpie object
  x <- as.quitte(xq) %>% as.magpie()
  
  # Converting EUR2015 to $2015
  x <- x * 1.1
  
  xq <- as.quitte(x)
  
  # Use IEA - OPENPROM mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-IEA-pgall-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")
  
  a <- readSource("IEA_WEO_TechCosts", convert = TRUE)
  
  #Converting $2022 to $2015
  a <- a * 0.81
  
  qa <- as.quitte(a)
  merged <- merge(map, qa, by.x = "IEA", by.y = "technology") # INNER JOIN
  
  scenario <- NULL
  
  merged <- filter(merged, scenario == "Stated Policies")
  merged <- filter(merged, variable == "Annual O&M Costs")
  
  # Renaming and dropping columns
  xqIEA <- select(merged, -c("PRIMES", "IEA", "variable"))
  xqIEA <- rename(xqIEA, "variable" = "OPEN.PROM")
  
  # Interpolating the missing values for the specified time period
  xqIEA <- interpolate_missing_periods(xqIEA, seq(fStartHorizon, fEndHorizon, 1), expand.values = TRUE)
  
  qx <- rbind(xq, xqIEA)
  
  region <- NULL
  period <- NULL
  
  qx <- qx %>% complete(variable, nesting(region, period))
  
  # Assign the global from RefScen where necessary
  qx <- filter(qx, region != "GLO")
  
  value.x <- NULL
  value.y <- NULL
  qx <- left_join(qx, xq, by = c("variable", "period")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y", "scenario.y", "region.y", "unit.y", "model.y"))
  names(qx) <- sub("region.x", "region", names(qx))
  names(qx) <- sub("scenario.x", "scenario", names(qx))
  names(qx) <- sub("unit.x", "unit", names(qx))
  names(qx) <- sub("model.x", "model", names(qx))
  
  # Converting to magpie object
  x <- as.quitte(qx) %>% as.magpie()
  # Set NA to 0
  x[is.na(x)] <- 0
  
  #fix units
  getItems(x,3.3) <- c("USD/kW", "USD/kW")
  
  list(x = x,
       weight = NULL,
       unit = "$2015/kW",
       description = "IEA and EU Reference Scenario 2020; Fixed O&M costs")
}
