#' calcOPENGEM_POP
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
#' POP <- calcOutput("OPENGEM_POP", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr filter %>%

calcOPENGEM_POP <- function(scenario = "SSP2") {
  
  OECD_SSP_POP <- readSource("OECD_SSP_POP", convert = TRUE)
  OECD_SSP_POP <- collapseDim(OECD_SSP_POP, 3.1)
  OECD_SSP_POP <- as.quitte(OECD_SSP_POP) %>%
    interpolate_missing_periods(period = 2010 : 2100, expand.values = TRUE) %>%
    as.magpie()
  getItems(OECD_SSP_POP, 3.2) <- "billions"
  getItems(OECD_SSP_POP, 3.1) <- scenario
  OECD_SSP_POP[is.na(OECD_SSP_POP)] <- 0
  OECD_SSP_POP <- OECD_SSP_POP / 1000
  
  OPEN_POP <- readSource("OPEN_GEM", convert = FALSE, subset = "GDP_POP")
  OPEN_GEM_POP <- OPEN_POP[,,"POP"]
  
  x_growth <- as.quitte(OPEN_GEM_POP) %>%
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
  
  POP <- OECD_SSP_POP
  
  years <- getYears(POP, as.integer = TRUE)
  
  for (yr in years[years > 2023]) {
    POP[, yr, ] <- POP[, yr - 1, ] * (1 + x_growth[, yr, ])
  }
  
  
  list(x = POP,
       weight = NULL,
       unit = "billion",
       description = "Population OPEN_GEM")
}
