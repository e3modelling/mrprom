#' calcTNewShareStockPC
#'
#' Derive car stock per fuel technology for all countries.
#' @param subtype Period that starts the projection.
#'
#' @return magpie object
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' TStockPC <- calcOutput("TNewShareStockPC", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter group_modify %>% mutate select rename group_by summarise ungroup inner_join full_join right_join recode first
#' @importFrom tidyr complete replace_na
#' @importFrom magclass as.magpie
#' @importFrom quitte as.quitte
#' @importFrom zoo na.locf
#'
calcTNewShareStockPC <- function() {
  # -------------- Historical ELC share sales -----------
  dataIEA_EV <- readSource("IEA_EV", convert = TRUE) %>%
    as.quitte() %>%
    filter(
      variable == "Cars",
      category == "Historical"
    ) %>%
    select(-c("variable", "category"))

  salesTechShares <- dataIEA_EV %>%
    filter(
      parameter %in% c("EV sales")
    ) %>%
    group_by(region, period, parameter) %>%
    mutate(value = value / sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(
      filter(dataIEA_EV, parameter == "EV sales share") %>% select(-powertrain),
      by = c("region", "period")
    ) %>%
    mutate(
      value = value.x * value.y / 100,
      value = ifelse(period <= 2020 & is.na(value), 0, value)
    ) %>%
    rename(tech = powertrain) %>%
    select(region, period, tech, value)

  # --------------- Targets --------------------------
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

  TsalesTechShares <- salesTechShares %>%
    left_join(mapping, by = "region") %>%
    group_by(region, tech, code) %>%
    complete(period = 2021:2100) %>%
    mutate(
      value = if_else(
        code == "LAM" & tech == "TELC" & period == 2026,
        0.27,
        value
      ),
      value = if_else(
        code == "LAM" & tech == "TELC" & period == 2033,
        0.4,
        value
      ),
      value = if_else(
        code == "LAM" & tech == "TH2F" & period == 2033,
        0.1,
        value
      ),
      value = if_else(
        code == "CAZ" & tech == "TELC" & period == 2035,
        0.8,
        value
      ),
      value = if_else(
        code == "CAZ" & tech == "TH2F" & period == 2035,
        0.2,
        value
      ),
      value = if_else(
        code == "EUR" & tech == "TELC" & period == 2035,
        0.8,
        value
      ),
      value = if_else(
        code == "EUR" & tech == "TH2F" & period == 2035,
        0.2,
        value
      ),
      value = if_else(
        code == "IND" & tech == "TELC" & period == 2030,
        0.25,
        value
      ),
      value = if_else(
        code == "IND" & tech == "TH2F" & period == 2030,
        0.05,
        value
      ),
      value = if_else(
        code == "JPN" & tech == "TELC" & period == 2035,
        0.10,
        value
      ),
      value = if_else(
        code == "JPN" & tech == "TH2F" & period == 2035,
        0.025,
        value
      ),
      value = if_else(
        code == "OAS" & tech == "TELC" & period == 2030,
        0.45,
        value
      ),
      value = if_else(
        code == "OAS" & tech == "TH2F" & period == 2030,
        0.05,
        value
      ),
      # Linear interpolation
      value = zoo::na.approx(value, x = period, na.rm = FALSE, rule = 2)
    ) %>%
    ungroup() %>%
    filter(period > 2020) %>%
    select(-code) %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill()

  salesTechShares <- salesTechShares %>%
    as.quitte() %>%
    as.magpie()

  weights <- TsalesTechShares
  weights[, , ] <- salesTechShares[, "y2020", ]
  weights[is.na(TsalesTechShares)] <- 0
  TsalesTechShares[is.na(TsalesTechShares)] <- -999

  list(
    x = TsalesTechShares,
    weight = weights,
    unit = "(1)",
    description = "Targets for share",
    aggregationArguments = list(zeroWeight = "setNA")
  )
}
