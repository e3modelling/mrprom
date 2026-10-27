#' calcOPENGEM_GDP
#'
#' @param scenario string. By choosing a scenario you filter the SSP dataset
#' by type.
#'
#' @return The SSP data filtered by gdp
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' gdp <- calcOutput("OPENGEM_GDP", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr %>% arrange group_by mutate case_when ungroup filter select lead
#' @importFrom tidyr uncount

calcOPENGEM_GDP <- function(scenario = "SSP2") {
  
  OECD_SSP_GDP <- readSource("OECD_SSP_GDP", convert = TRUE)
  OECD_SSP_GDP <- as.quitte(OECD_SSP_GDP) %>%
    interpolate_missing_periods(period = 2010 : 2100, expand.values = TRUE) %>%
    as.magpie()
  getItems(OECD_SSP_GDP, 3.2) <- "GDP|PPP_billion US$2015/yr"
  getItems(OECD_SSP_GDP, 3.1) <- scenario
  OECD_SSP_GDP[is.na(OECD_SSP_GDP)] <- 0
  
  OPEN_GEM <- readSource("OPEN_GEM", convert = FALSE, subset = "GDP_POP")
  OPEN_GEM_GDP <- OPEN_GEM[,,"GDP"]
  
  x_growth <- as.quitte(OPEN_GEM_GDP) %>%
    arrange(region, variable, period) %>%
    group_by(region, variable) %>%
    mutate(
      next_period = lead(period),
      next_value = lead(value),
      annual_growth = case_when(
        value == 0 & next_value == 0 ~ 0,
        value <= 0 | next_value <= 0 ~ NA_real_,
        TRUE ~ (next_value / value)^(1 / (next_period - period)) - 1
      )
    ) %>%
    ungroup() %>%
    filter(!is.na(next_period)) %>%
    uncount(next_period - period, .id = "step") %>%
    mutate(
      period = period + step,
      value = annual_growth
    ) %>%
    select(-next_period, -next_value, -annual_growth, -step) %>%
    filter(period > 2017)
  
  x_growth <-  as.quitte(x_growth) %>%
    interpolate_missing_periods(period = 2010 : 2100, expand.values = TRUE)
  
  x_growth <- as.quitte(x_growth) %>% as.magpie()
  
  GDP <- OECD_SSP_GDP
  
  years <- getYears(GDP, as.integer = TRUE)
  
  for (yr in years[years > 2023]) {
    GDP[, yr, ] <- GDP[, yr - 1, ] * (1 + x_growth[, yr, ])
  }
  
  list(x = GDP,
       weight = NULL,
       unit = "various",
       description = "GDP|PPP; based on OECD SSP2 historical data and OPEN-GEM growth rates after 2023")
}
