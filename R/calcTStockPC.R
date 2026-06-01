#' calcTStockPC
#'
#' Derives passenger car stock shares by vehicle technology for all countries
#' based on historical stock composition and predefined technology transition
#' targets. Historical vehicle stocks are converted into technology shares,
#' which are then projected forward using region-specific assumptions on the
#' phase-out of internal combustion engine (ICE) vehicles.
#' For European countries, ICE vehicle technologies are assumed to decline to
#' 50% of their 2020 stock share by 2030 and to reach zero share by 2050.
#' For countries in the OAS region, ICE vehicle technologies are assumed to
#' reach zero share by 2040. Intermediate years are generated through linear
#' interpolation, while remaining missing values are forward-filled to ensure
#' complete trajectories over the model horizon.
#' Historical stock shares are used as aggregation weights, allowing
#' technology-specific targets to be consistently aggregated across regions.
#' The resulting dataset provides country-level passenger car stock shares by
#' technology from 2021 onwards.
#' 
#' @param subtype Period that starts the projection.
#'
#' @return magpie object
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' TStockPC <- calcOutput("TStockPC", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter group_modify %>% mutate select rename group_by summarise ungroup inner_join full_join right_join recode first
#' @importFrom tidyr complete replace_na
#' @importFrom magclass as.magpie
#' @importFrom quitte as.quitte
#' @importFrom zoo na.locf na.approx
#'
calcTStockPC <- function() {
  techsICE <- c(
    "TGSL", "TLPG", "TGDO", "TNGS", "TETH",
    "TPHEVGSL", "TPHEVGDO", "TCHEVGSL", "TCHEVGDO"
  )

  stockPC <- calcOutput("StockPC", aggregate = FALSE) + 1e-6

  stockShare <- stockPC %>%
    as.quitte() %>%
    select(region, period, tech, value) %>%
    group_by(region, period) %>%
    mutate(
      total = sum(value, na.rm = TRUE),
      value = value / total
    ) %>%
    ungroup() %>%
    select(-total)

  # --------------- EU targets --------------------------
  mapEurope <- toolGetMapping(
    name = "regionmappingOP4.csv",
    type = "regional",
    where = "mrprom"
  ) %>%
    select(-Full.Country.Name) %>%
    rename(region = ISO3.Code, code = Region.Code)

  mapping <- toolGetMapping(
    name = "regionmappingOPDEV5.csv",
    type = "regional",
    where = "mrprom"
  ) %>%
    rename(region = ISO3.Code, code = Region.Code) %>%
    left_join(mapEurope, by = "region") %>%
    mutate(code = ifelse(code.y == "EUR", code.y, code.x)) %>%
    select(-c("Full.Country.Name", "code.x", "code.y"))

  TstockShare <- stockShare %>%
    left_join(mapping, by = "region") %>%
    group_by(region, tech, code) %>%
    complete(period = 2021:2100) %>%
    mutate(
      value = if_else(
        code == "EUR" & tech %in% techsICE & period == 2030,
        0.5 * value[period == 2020],
        value
      ),
      value = if_else(
        code == "EUR" & tech %in% techsICE & period == 2050,
        0,
        value
      ),
      value = if_else(
        code == "OAS" & tech %in% techsICE & period == 2040,
        0,
        value
      ),
      # Linear interpolation
      value = if_else(period > 2020,
        na.approx(value, x = period, na.rm = FALSE),
        value
      ),
      # replace remaining NA with the last non‑NA value (or keep NA)
      value = na.locf(value, na.rm = FALSE)
    ) %>%
    ungroup() %>%
    filter(period > 2020) %>%
    select(-code) %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill()

  weights <- TstockShare
  weights[, , ] <- stockPC[, "y2020", ]
  weights[is.na(TstockShare)] <- 0
  TstockShare[is.na(TstockShare)] <- -999

  # aa <- toolAggregate(TstockShare, dim = 1,rel = mapping, from = "region", to = "code", weight = weights, zeroWeight = "setNA", mixed_aggregation = FALSE)

  list(
    x = TstockShare,
    weight = weights,
    unit = "(1)",
    description = "Targets for share",
    aggregationArguments = list(zeroWeight = "setNA")
  )
}
