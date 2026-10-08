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

  a <- calcOutput("GDP", scenario = scenario, aggregate = FALSE) * 0.97 / 1000#convert to USD_2015
  a <- as.quitte(a) %>% interpolate_missing_periods(period = seq(2010, 2100, 1), expand.values = TRUE)
  a["variable"] <- scenario
  a <- filter(a, period %in% c(2010 : 2100))
  a[["unit"]] <- "GDP|PPP.billion US$2015/yr"
  a <- as.quitte(a) %>% as.magpie()
  a[is.na(a)] <- 0
  
  GDP_MultiFutures <- readSource("MultiFutures", subtype = "GDP")
  
  map <- toolGetMapping(("regionmappingOPDEV5.csv"), "regional", where = "mrprom")
  
  GDP_MultiFutures <- toolAggregate(
    GDP_MultiFutures,
    weight = NULL,
    dim = 1,
    rel = map,
    from = "Region.Code",
    to = "ISO3.Code"
  )
  
  getItems(GDP_MultiFutures,3.1) <- "GDP|PPP"
  getItems(GDP_MultiFutures,3.2) <- "billion US$2015/yr"
  
  # Start with original GDP
  result <- a
  
  # Common regions
  common_regions <- intersect(
    getRegions(a),
    getRegions(GDP_MultiFutures)
  )
  
  # Years from 2024 onwards
  years <- intersect(
    getYears(a),
    getYears(GDP_MultiFutures)
  )
  
  years <- years[as.integer(sub("y", "", years)) >= 2024]
  years <- years[order(as.integer(sub("y", "", years)))]
  
  # Calculate GDP recursively
  for (year in years) {
    
    previous_year <- paste0(
      "y", as.integer(sub("y", "", year)) - 1
    )
    
    result[common_regions, year, ] <-
      result[common_regions, previous_year, ] *
      GDP_MultiFutures[common_regions, year, ]
  }
  
  a <- result
  
  list(x = a,
       weight = NULL,
       unit = "billion US$2015/yr",
       description = "GDP|PPP; Source: SSP Scenarios mrdrivers")
}
