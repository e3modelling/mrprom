#' convertIEA_EV
#'
#' The ISO codes of "IEA_EV" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "IEA_EV" data with spatial entries for each country.
#'
#' @author Fotis Sioutas, Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_EV", convert = TRUE)
#' }
#'
#' @importFrom dplyr filter %>% pull
convertIEA_EV <- function(x) {
  # Fill all countries
  y <- add_columns(x,
    addnm = setdiff(getISOlist(), getItems(x, 1)),
    dim = 1,
    fill = NA
  )
  
  countries <- toolGetMapping("regionmappingH12.csv", where = "madrat") %>%
    filter(RegionCode != "EUR", !CountryCode %in% c("USA", "IND", "CHN")) %>%
    pull(CountryCode)

  countriesEU <- toolGetMapping("regionmappingH12.csv", where = "madrat") %>%
    filter(RegionCode == "EUR") %>%
    select(CountryCode) %>%
    pull()

  # Use aggregate shares for each country
  y[countries, "y2030", c("EV stock share", "EV sales share")] <- y["RWRL", "y2030", c("EV stock share", "EV sales share")]
  y[countriesEU, "y2030", c("EV stock share", "EV sales share")] <- y["EUR", "y2030", c("EV stock share", "EV sales share")]


  mappingEVs <- list(
    "BEV" = "TELC",
    "PHGDO" = "TPHEVGDO",
    "PHGSL" = "TPHEVGSL",
    "FCEV" = "TH2F"
  )

  z <- as.quitte(y) %>%
    filter(
      !(category == "Historical" & period >= 2025),
      !(category == "Projection-STEPS" & period < 2025)
    ) %>%
    select(-category) %>%
    splitPHEV() %>%
    # Apply renaming
    mutate(
      powertrain = recode(powertrain, !!!mappingEVs),
      # Change % to decimals
      value = ifelse(unit == "percent", value / 100, value)
    ) %>%
    disaggregateShares() %>%
    as.quitte() %>%
    replace_na(list(value = 0)) %>%
    interpolate_missing_periods(period = seq(2010, 2030, 1)) %>%
    as.magpie()
  return(z)
}

# ---------------------- Helpers ----------------------------------
splitPHEV <- function(y) {
  # Split PHEV uniformly to PHEVGSL and PHEVGDO
  z <- y %>%
    mutate(
      powertrain = as.character(powertrain),
      powertrain = ifelse(powertrain == "PHEV", "PHGSL", powertrain),
      value = ifelse(powertrain == "PHGSL", value / 2, value)
    ) %>%
    bind_rows(
      y %>%
        filter(powertrain == "PHEV") %>%
        mutate(
          powertrain = "PHGDO",
          value = value / 2
        )
    )
  return(z)
}

disaggregateShares <- function(y) {
  # Dissagregate ELC shares to various techs (TELC, TH2F, TPHEVGSL, TPGEVGDO)
  sharesTech <- y %>%
    filter(
      parameter %in% c("EV sales", "EV stock"),
    ) %>%
    group_by(region, period, variable, parameter) %>%
    replace_na(list(value = 0)) %>%
    mutate(
      value = (value + 1e-6) / sum(value + 1e-6, na.rm = TRUE),
      parameter = ifelse(parameter == "EV stock", "EV stock share", "EV sales share")
    ) %>%
    ungroup() %>%
    select(-c("unit"))

  sharesTechLookUp <- sharesTech %>%
    filter(
      region %in% c("EUR", "RWRL"),
      period >= 2025
    )

  sharesTech <- sharesTech %>%
    left_join(
      toolGetMapping("regionmappingH12.csv", where = "madrat"),
      by = c("region" = "CountryCode")
    ) %>%
    mutate(RegionCode = ifelse(RegionCode %in% c("CHA", "IND", "USA", "EUR"), RegionCode, "RWRL")) %>%
    left_join(
      sharesTechLookUp,
      by = c("RegionCode" = "region", "period", "variable", "parameter", "powertrain")
    ) %>%
    mutate(
      value = ifelse(is.na(value.y), value.x, value.y),
      value = ifelse(value > 1e-6, value, 0)
    ) %>%
    select(region, period, variable, parameter, powertrain, value)

  final <- y %>%
    filter(region %in% unname(getISOlist())) %>%
    left_join(sharesTech, by = c("region", "period", "variable", "parameter")) %>%
    mutate(
      value = value.x * value.y,
      powertrain = ifelse(is.na(powertrain.y), powertrain.x, powertrain.y)
    ) %>%
    select(region, period, variable, unit, parameter, powertrain, value)
  return(final)
}
