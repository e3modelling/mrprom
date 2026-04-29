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
    # Step 3: drop BASE rows
    filter(scenario != "BASE") %>%
    # Step 4: clean up helper column
    select(-base_value)
  
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = 2020 : 2100, expand.values = TRUE)
  
  x <- as.quitte(qx) %>% as.magpie()
  
  x[is.na(x)] <- 0
  
  list(x = x,
       weight = NULL,
       unit = "TWh",
       description = "Consumption of data centers")
}
