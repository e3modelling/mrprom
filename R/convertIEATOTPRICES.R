#' convertIEATOTPRICES
#'
#' The ISO codes of "IEATOTPRICES" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "IEAEnergyPrices" data with spatial entries for each country.
#'
#' @author Michael Madianos, Fotis Sioutas
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEATOTPRICES", convert = TRUE)
#' }
#'
convertIEATOTPRICES <- function(x) {
  # Impute missing regions based on the prices of its continent
  regionsOPENPROM <- unname(getISOlist())

  # Mapping from regions to IEA continent names (GRC -> EUROPE)
  regionsToContinents <- toolGetMapping(
    name = "regionMappingContinentsIEA.csv",
    type = "regional",
    where = "mrprom"
  ) %>%
    select(-c("Region.Code", "Full.Country.Name")) %>%
    left_join(data.frame(ISO3.Code = regionsOPENPROM), by = "ISO3.Code") %>%
    rename(region = ISO3.Code, continent = Continent.Code)

  continents <- unique(regionsToContinents$continent)
  # IEA price values for each continent
  continentsLookUp <- x %>%
    as.quitte() %>%
    filter(region %in% continents) %>%
    rename(continent = region) %>%
    select(continent, period, variable, fuel, value)

  unavailableRegions <- setdiff(regionsOPENPROM, getRegions(x))
  keep <- intersect(regionsOPENPROM, getRegions(x))
  x <- x[keep, , ] # keep only available countries (not continents/aggregated regions)
  suppressWarnings({
    x <- toolCountryFill(x, fill = NA) %>%
      as.quitte() %>%
      left_join(regionsToContinents, by = "region") %>%
      left_join(
        continentsLookUp,
        by = c("continent", "period", "variable", "fuel")
      ) %>%
      mutate(
        # If region is not available; Use continent prices
        value = ifelse(region %in% unavailableRegions, value.y, value.x),
        # If region available but no values; Use continent prices
        value = ifelse(is.na(value.x), value.y, value.x)
      ) %>%
      select(region, period, variable, fuel, unit, value) %>%
      as.quitte() %>%
      as.magpie()
  })
  

  x <- helperConvertUnits(x)
  return(x[as.character(getISOlist()), , ])
}
# -----------------------------------------------------------------------------
helperConvertUnits <- function(x) {
  # Change of units to $2015/toe
  x[, , "CURR_MWH"] <- x[, , "CURR_MWH"] * 11.63
  x[, , "CURR_MWH_GCV"] <- x[, , "CURR_MWH_GCV"] * 11.63

  x[, , "CURR_LITRE"][, , c("DIESEL_ROAD")] <- x[, , "CURR_LITRE"][, , c("DIESEL_ROAD")] * 1085
  x[, , "CURR_LITRE"][, , c("LPG")] <- x[, , "CURR_LITRE"][, , c("LPG")] * 1642.7
  x[, , "CURR_LITRE"][, , c("GASOLINE", "MOTORGAS_UNLEAD_REG", "MOTORGAS_UNLEAD_HIGHOCT", "MOTORGAS_UNLEAD_MIDOCT", "MOTORGAS_LEAD_REG", "MOTORGAS_LEAD_PREM")] <- x[, , "CURR_LITRE"][, , c("GASOLINE", "MOTORGAS_UNLEAD_REG", "MOTORGAS_UNLEAD_HIGHOCT", "MOTORGAS_UNLEAD_MIDOCT", "MOTORGAS_LEAD_REG", "MOTORGAS_LEAD_PREM")] * 1224.04
  x[, , "CURR_LITRE"][, , c("KEROSENE_JET")] <- x[, , "CURR_LITRE"][, , c("KEROSENE_JET")] * 1196.2

  x[, , "CURR_K_LITRE"][, , "LFO"] <- x[, , "CURR_K_LITRE"][, , "LFO"] * 1.1692
  x[, , "CURR_K_LITRE"][, , "KEROSENE"] <- x[, , "CURR_K_LITRE"][, , "KEROSENE"] * 1.1962

  x[, , "CURR_TONNE"][, , c("CHARCOAL", "STEAMCOAL", "COKING_COAL")] <- x[, , "CURR_TONNE"][, , c("CHARCOAL", "STEAMCOAL", "COKING_COAL")] * 1.7445
  x[, , "CURR_TONNE"][, , c("FUEL_OIL_RESIDUAL")] <- x[, , "CURR_TONNE"][, , c("FUEL_OIL_RESIDUAL")] * 1.0363
  x[, , "CURR_TONNE"][, , c("FUEL_OIL_HIGH_SULPHUR", "FUEL_OIL_LOW_SULPHUR")] <- x[, , "CURR_TONNE"][, , c("FUEL_OIL_HIGH_SULPHUR", "FUEL_OIL_LOW_SULPHUR")] / 0.9782
  x[, , "CURR_TONNE"][, , c("PRIMARY_SOLID_BIOFUEL")] <- x[, , "CURR_TONNE"][, , c("PRIMARY_SOLID_BIOFUEL")] * 2.7912

  getItems(x, 3.2) <- rep("USD2015/toe", length(getItems(x, 3.2)))
  return(x)
}
