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
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr %>% filter

calciGDP <- function(scenario = "SSP2") {
  # 
  # a <- calcOutput("GDP", scenario = scenario, aggregate = FALSE) * 0.97 / 1000#convert to USD_2015
  # a <- as.quitte(a) %>% interpolate_missing_periods(period = seq(2010, 2100, 1), expand.values = TRUE)
  # a["variable"] <- scenario
  # a <- filter(a, period %in% c(2010 : 2100))
  # a[["unit"]] <- "GDP|PPP.billion US$2015/yr"
  # a <- as.quitte(a) %>% as.magpie()
  # a[is.na(a)] <- 0
  
  OECD_SSP_GDP <- readSource("OECD_SSP_GDP", convert = TRUE)
  OECD_SSP_GDP <- as.quitte(OECD_SSP_GDP) %>%
    interpolate_missing_periods(period = 2010 : 2100, expand.values = TRUE) %>%
    as.magpie()
  getItems(OECD_SSP_GDP, 3.2) <- "GDP|PPP_billion US$2015/yr"
  getItems(OECD_SSP_GDP, 3.1) <- scenario
  OECD_SSP_GDP[is.na(OECD_SSP_GDP)] <- 0
  
  list(x = OECD_SSP_GDP,
       weight = NULL,
       unit = "billion US$2015/yr",
       description = "GDP|PPP; IIASA-Wic POP 2025 SSPx & Historical Reference 
OECD ENV-Growth 2025 SSPx & Historical Reference USD2015")
}
