#' calciGDP_MER
#'
#' "Country-level GDP at Market Exchange Rates (MER), derived from",
#' "SSP GDP projections (mrdrivers) expressed in Purchasing Power Parity (PPP).",
#' "PPP GDP is converted to MER using the World Bank Price Level Index.",
#' "Values are expressed in billion constant US$2015 per year,",
#' "with annual interpolation for 2010–2100."
#'
#' @param scenario string. By choosing a scenario you filter the SSP dataset
#' by type.
#'
#' @return The SSP data filtered by gdp
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' gdp <- calcOutput("iGDP_MER", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr %>% filter

calciGDP_MER <- function(scenario = "SSP2") {
  
  a <- calcOutput("GDP", scenario = scenario, aggregate = FALSE) * 0.97 / 1000#convert to USD_2015
  a <- as.quitte(a) %>% interpolate_missing_periods(period = seq(2010, 2100, 1), expand.values = TRUE)
  a["variable"] <- scenario
  a <- filter(a, period %in% c(2010 : 2100))
  a[["unit"]] <- "GDP|PPP.billion US$2015/yr"
  a <- as.quitte(a) %>% as.magpie()
  a[is.na(a)] <- 0
  
  WorldBankPriceLevelIndex <- readSource("WorldBankPriceLevelIndex")
  
  a <- a * WorldBankPriceLevelIndex
  
  a <- collapseDim(a, 2.2)
  a <- collapseDim(a, 3.3)
  getItems(a, 3.2) <- "GDP|MER_billion US$2015/yr"
  
  list(x = a,
       weight = NULL,
       unit = "billion US$2015/yr",
       description = "GDP|MER; Source: SSP Scenarios mrdrivers")
}
