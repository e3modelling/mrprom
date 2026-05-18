#' calcIFuelConsICT
#'
#' @return Consumption of data centers by scenario and variable.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput("IFuelConsICT", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr %>% filter select left_join mutate if_else

calcIFuelConsICT <- function() {
  
  a <- readSource("ICTPrisma", subtype = "Consumption of data centers")
  q <- as.quitte(a)
  
  # NZL has negative SE ELC in fuel consumption so add 2023 is equal to 2020
  qNZL <- as.quitte(a["NZL",2020,])
  
  x <- q %>%
    # Step 1: isolate BASE values
    left_join(
      q %>%
        filter(scenario == "BASE") %>%
        select(region, period, variable, unit, base_value = value),
      by = c("region", "period", "variable", "unit")
    ) %>%
    # Step 2: fill missing values in other scenarios using BASE
    mutate(
      value = if_else(scenario != "BASE" & is.na(value), base_value, value)
    ) %>%
    # Step 4: drop BASE rows
    filter(scenario != "BASE") %>%
    # Step 4: clean up helper column
    select(-base_value)
  
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = 2010 : 2100, expand.values = TRUE) %>%
    # NZL has negative SE ELC in fuel consumption so add 2023 is equal to 2020
    mutate(
      value = if_else(region == "NZL" & (period %in% c(2021 : 2023)), unique(qNZL[["value"]][!is.na(qNZL[["value"]])]), value)
    )
  
  qx <- qx %>%
    mutate(
      variable = case_when(
        variable == "Lower Bound DC" ~ "Lower",
        variable == "Upper Bound DC" ~ "Upper",
        variable == "Central Estimate DC" ~ "Central",
        variable == "Mean DC" ~ "Mean",
        TRUE ~ variable
      ),
      value = value * 0.086, # TWh to Mtoe
      unit = "Mtoe"
    )
  
  x <- as.quitte(qx) %>% as.magpie()
  
  # Option 1 (SELECTED) from R12_Clean IAM Version_Finalised_2100_update_2026-03-31.xlsx
  # Upper ICT = Upper bound DC + 0.91 ratio
  # Lower ICT = Lower bound DC + 0.78 ratio
  # Mean = Average DC + 0.78 ratio
  # Central Estimate = Central DC + 0.78 ratio
  # Historical = Historical + 0.91 ratio until 2023
  
  x[,2010:2023,] <-  x[,2010:2023,] * 1.91
  x[,setdiff(getYears(x, as.integer = TRUE),2010:2023),"Upper"] <-  x[,setdiff(getYears(x, as.integer = TRUE),2010:2023),"Upper"] * 1.91
  x[,setdiff(getYears(x, as.integer = TRUE),2010:2023),"Lower"] <-  x[,setdiff(getYears(x, as.integer = TRUE),2010:2023),"Lower"] * 1.78
  x[,setdiff(getYears(x, as.integer = TRUE),2010:2023),"Central"] <- x[,setdiff(getYears(x, as.integer = TRUE),2010:2023),"Central"] * 1.78
  x[,setdiff(getYears(x, as.integer = TRUE),2010:2023),"Mean"] <-  x[,setdiff(getYears(x, as.integer = TRUE),2010:2023),"Mean"] * 1.78
  
  x[is.na(x)] <- 0
  
  list(x = x,
       weight = NULL,
       unit = "Mtoe",
       description = "Consumption of data centers")
}
