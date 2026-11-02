#' calcPOP
#'
#' Derive population data based on SSP scenarios.
#' The data is sourced externally via calcOutput("Population") from the mrdrivers library,
#' and converted from millions to billions, with missing years interpolated to provide a
#' complete time series from 2010 to 2100.
#'
#' @param scenario SSP scenario choice
#' Available scenario are:
#' \itemize{
#' \item `SSP1`:
#' \item `SSP2`:
#' \item `SSP3`:
#' \item `SSP4`:
#' \item `SSP5`:
#' }
#'
#' @return The SSP data filtered by pop
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' POP <- calcOutput("POP", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>% arrange group_by mutate case_when lead ungroup filter select bind_rows
#' @importFrom tidyr uncount

calcPOP <- function(scenario = "SSP2") {
  
  OECD_SSP_POP <- readSource("OECD_SSP_POP", convert = TRUE)
  OECD_SSP_POP <- collapseDim(OECD_SSP_POP, 3.1)
  
  x <- as.quitte(OECD_SSP_POP)
  
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
  
  OECD_SSP_POP <- bind_rows(x, x_growth) %>%
    arrange(region, variable, period) %>%
    as.quitte() %>%
    as.magpie()
  
  getItems(OECD_SSP_POP, 3.2) <- "billions"
  getItems(OECD_SSP_POP, 3.1) <- scenario
  
  OECD_SSP_POP[is.na(OECD_SSP_POP)] <- 0
  OECD_SSP_POP <- OECD_SSP_POP / 1000
  
  # p <- calcOutput("Population", scenario = scenario, aggregate = FALSE) / 1000 #billions
  # p <- as.quitte(p) %>% interpolate_missing_periods(period = seq(2010, 2100, 1), expand.values = TRUE)
  # p["variable"] <- scenario
  # p <- filter(p, period %in% c(2010 : 2100))
  # p[["unit"]] <- "billions"
  # p <- as.quitte(p) %>% as.magpie()
  # p[is.na(p)] <- 0
  
  # OECD_SSP_POP <- readSource("OECD_SSP_POP", convert = TRUE)
  # OECD_SSP_POP <- collapseDim(OECD_SSP_POP, 3.1)
  # OECD_SSP_POP <- as.quitte(OECD_SSP_POP) %>%
  #   interpolate_missing_periods(period = 2010 : 2100, expand.values = TRUE) %>%
  #   as.magpie()
  # getItems(OECD_SSP_POP, 3.2) <- "billions"
  # getItems(OECD_SSP_POP, 3.1) <- scenario
  # OECD_SSP_POP[is.na(OECD_SSP_POP)] <- 0
  # OECD_SSP_POP <- OECD_SSP_POP / 1000

  list(x = OECD_SSP_POP,
       weight = NULL,
       unit = "billion",
       description = "Population; IIASA-Wic POP 2025 SSPx & Historical Reference 
OECD ENV-Growth 2025 SSPx & Historical Reference USD2015")
}
