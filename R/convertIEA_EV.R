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
  y <- add_columns(x, addnm = setdiff(getISOlist(), getItems(x, 1)), dim = 1, fill = NA)

  countries <- toolGetMapping("regionmappingH12.csv", where = "madrat") %>%
    filter(RegionCode != "EUR", !CountryCode %in% c("USA", "IND", "CHN")) %>%
    pull(CountryCode)

  y[countries, getYears(y)[getYears(y) > "y2021"], "EV stock share"] <- y["RWRL", getYears(y)[getYears(y) > "y2021"], "EV stock share"]
  y <- y[as.character(getISOlist()), , ] %>%
    as.quitte() %>%
    interpolate_missing_periods(period = seq(2010, 2030, 1)) %>%
    mutate(powertrain = as.character(powertrain))

  # Split PHEV in powertrain to PHGSL and PHGDO uniformly
  z <- y %>%
    mutate(
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

  mappingEVs <- list(
    "BEV" = "TELC",
    "PHEVGDO" = "TPHEVGDO",
    "PHEVGSL" = "TPHEVGSL",
    "FCEV" = "TH2F"
  )

  z <- z %>%
    # Apply renaming
    mutate(powertrain = recode(powertrain, !!!mappingEVs)) %>%
    as.quitte() %>%
    as.magpie()
  return(z)
}
