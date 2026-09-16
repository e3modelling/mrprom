#' calcIECEMF
#' 
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IECEMF", aggregate = TRUE)
#' }
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr filter select

calcIECEMF <- function() {
  
  ECEMF <- readSource("ECEMF")
  
  x <- ECEMF
  # complete incomplete time series
  x <- as.quitte(x) %>%
    mutate(period = as.integer(period)) %>%
    interpolate_missing_periods(
      period = 2010 : 2100,
      expand.values = TRUE
    ) %>%
    filter(period >= 2010) %>%
    as.magpie()
  
  EU27 <- x[ "EU27",, ]
  World <- x[ "World",, ]
  
  map <- toolGetMapping(name = "EU28.csv",
                        type = "regional",
                        where = "mrprom") %>%
    filter(Region.Code != "GBR")
  
  # Create NEW magpie with all 249 countries
  x_new <- new.magpie(
    cells_and_regions = getISOlist(),
    years = getYears(World),
    names = getNames(World),
    fill = NA
  )
  
  # Give World values to ALL countries first
  for (r in getISOlist()) {
    x_new[r, , ] <- World["World", , ]
  }
  
  # Overwrite the 27 EU countries with EU27 values
  for (r in map$ISO3.Code) {
    x_new[r, , ] <- EU27["EU27", , ]
  }
  
  x <- x_new
  
  weights <- calcOutput("iGDP", aggregate = FALSE)
  
  list(x = x,
       weight = weights,
       unit = "%",
       description = "CO2_emissions data")
}
