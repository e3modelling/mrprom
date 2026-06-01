#' calcIGrossCapCosSubRen
#'
#' Imports and processes power generation capital cost data from the EU Reference
#' Scenario and the IEA World Energy Outlook technology cost database to create the
#' OPEN-PROM input parameter {iGrossCapCosSubRen}. The dataset represents
#' overnight investment costs for power generation technologies and is expressed in
#' constant 2015 U.S. dollars per kilowatt {$2015/kW}.
#' Capital cost projections are first extracted from the EU Reference Scenario
#' technology cost database and mapped to the corresponding OPEN-PROM power
#' generation technologies. Only overnight investment costs are retained. Missing
#' technology values not available in the EU Reference Scenario are supplemented
#' with literature-based assumptions, and all time series are interpolated to
#' provide annual values across the model horizon. Since the original EU Reference
#' Scenario data are reported in EUR2015, values are converted to USD2015 using a
#' fixed exchange rate.
#' Additional technology cost information is obtained from the IEA World Energy
#' Outlook technology cost database. Capital costs from the Stated Policies
#' Scenario are selected, mapped to OPEN-PROM technologies, converted from
#' 2022 U.S. dollars to 2015 U.S. dollars, and interpolated to annual values over
#' the modeling horizon.
#' The two datasets are then combined to maximize technology coverage. Whenever
#' capital cost data are available from both sources for the same technology and
#' year, the IEA value is preferred. For technologies or periods where IEA data are
#' unavailable, values from the EU Reference Scenario are used as a fallback.
#' Remaining gaps are filled using the corresponding global values from the EU
#' Reference Scenario, ensuring complete regional coverage for all modeled
#' technologies.
#' The resulting dataset provides technology-specific capital costs for power and
#' heat generation technologies and serves as an input dataset for the OPEN-PROM
#' modeling framework.
#'
#' @return magpie object with OPENPROM input data iGrossCapCosSubRen.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IGrossCapCosSubRen", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select filter rename mutate case_when
#' @importFrom tidyr pivot_wider spread gather complete expand
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIGrossCapCosSubRen <- function() {

  x <- readSource("TechCosts2024", "PowerAndHeat", convert = TRUE)

  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fEndHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndHorizon"]

  # Use PRIMES - OPENPROM mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-primes-pgall-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")

  if (any(grepl("Overnight Investment Costs", getItems(x, 3.1)))) {
    getItems(x, 3.1)[grepl("Overnight Investment Costs", getItems(x, 3.1))] <- "Overnight Investment Costs"
  }
  xq <- as.quitte(x)
  merged <- merge(map, xq, by.x = "PRIMES", by.y = "technology") # INNER JOIN

  variable <- NULL
  # Filtering the variable cost per plant type rows and dropping unnecessary columns
  xq <- filter(merged, variable == "Overnight Investment Costs")
  xq <- xq %>% select(-c("PRIMES", "variable")) %>%
               rename("variable" = "OPEN.PROM")

  # Replacing zero values with 1e-6 to avoid bugs in GAMS
  xq <- xq %>%
    mutate(value = case_when(
      value == 0 ~ 0.000001, TRUE ~ value))

  # FIXME: Some power plant types are missing from EU Reference Scenario 2020
  #from: https://link.springer.com/chapter/10.1007/978-3-030-86884-0_6
  df_missing <- data.frame(
  variable = c("ATHOIL", "ATHOIL", "ATHOIL", "ATHOIL"),
  model = rep("(Missing)", 4),
  scenario = rep("(Missing)", 4),
  region = rep("GLO", 4),
  unit = rep("$2015/kW", 4),
  period = c(2020, 2030, 2040, 2050),
  value = c(1000, 1000, 1000, 1000))
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
  merged <- filter(merged, variable == "Capital costs")
  
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
       description = "IEA and EU Reference Scenario 2020; Capital Cost")
}
