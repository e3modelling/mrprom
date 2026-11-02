#' calciGDP
#'
#' Derives country-level GDP projections from the SSP socioeconomic
#' scenarios provided by the mrdrivers package. The selected SSP pathway
#' (default: SSP2) is retrieved from the GDP dataset, converted from
#' million to billion PPP-adjusted US$2015 per year, and adjusted from
#' US$2017 to US$2015 prices using a conversion factor of 0.97.
#' Annual GDP values are generated through interpolation to provide
#' continuous time series from 2010 to 2100. The selected SSP scenario
#' is stored as the variable dimension, producing a complete set of
#' country-level GDP trajectories for use as OPEN-PROM input data.
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
#' gdp <- calcOutput("iGDP", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>% arrange group_by mutate case_when lead ungroup filter select bind_rows
#' @importFrom tidyr uncount

calciGDP <- function(scenario = "SSP2") {
  
  OECD_SSP_GDP <- readSource("OECD_SSP_GDP", convert = TRUE)
  
  x <- as.quitte(OECD_SSP_GDP)
  
  x_growth <- x %>%
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
    filter(next_period - period > 1) %>%
    uncount(next_period - period - 1, .id = "step") %>%
    mutate(
      period = period + step,
      value = value * (1 + annual_growth)^step
    ) %>%
    select(-next_period, -next_value, -annual_growth, -step)
  
  OECD_SSP_GDP <- bind_rows(x, x_growth) %>%
    arrange(region, variable, period) %>%
    as.quitte() %>%
    as.magpie()
  
  getItems(OECD_SSP_GDP, 3.2) <- "GDP|PPP_billion US$2015/yr"
  getItems(OECD_SSP_GDP, 3.1) <- scenario
  
  OECD_SSP_GDP[is.na(OECD_SSP_GDP)] <- 0
  
  # 
  # a <- calcOutput("GDP", scenario = scenario, aggregate = FALSE) * 0.97 / 1000#convert to USD_2015
  # a <- as.quitte(a) %>% interpolate_missing_periods(period = seq(2010, 2100, 1), expand.values = TRUE)
  # a["variable"] <- scenario
  # a <- filter(a, period %in% c(2010 : 2100))
  # a[["unit"]] <- "GDP|PPP.billion US$2015/yr"
  # a <- as.quitte(a) %>% as.magpie()
  # a[is.na(a)] <- 0
  
  # OECD_SSP_GDP <- readSource("OECD_SSP_GDP", convert = TRUE)
  # OECD_SSP_GDP <- as.quitte(OECD_SSP_GDP) %>%
  #   interpolate_missing_periods(period = 2010 : 2100, expand.values = TRUE) %>%
  #   as.magpie()
  # getItems(OECD_SSP_GDP, 3.2) <- "GDP|PPP_billion US$2015/yr"
  # getItems(OECD_SSP_GDP, 3.1) <- scenario
  # OECD_SSP_GDP[is.na(OECD_SSP_GDP)] <- 0
  # 
  
  list(x = OECD_SSP_GDP,
       weight = NULL,
       unit = "billion US$2015/yr",
       description = "GDP|PPP; IIASA-Wic POP 2025 SSPx & Historical Reference 
OECD ENV-Growth 2025 SSPx & Historical Reference USD2015")
}
