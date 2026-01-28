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
  # -------------- IEA ELC share sales -----------
  salesTechShares <- readSource("IEA_EV", convert = TRUE) %>%
    as.quitte() %>%
    filter(
      variable == "Cars",
      parameter %in% c("EV sales share")
    ) %>%
    select(c("region", "period", "powertrain", "value")) %>%
    rename(tech = powertrain)

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
    # Match with PBL targets for NPI climate policies
    mutate(
      value = if_else(
        region == "BRA",
        3 * value,
        value
      ),
      value = ifelse(region == "CAN", 4 * value, value),
      value = if_else(
        region == "CAN" & tech == "TELC" & period == 2035,
        0.95,
        value
      ),
      value = if_else(
        region == "CAN" & tech == "TH2F" & period == 2035,
        0.05,
        value
      ),
      value = if_else(
        code == "EUR" & tech == "TELC" & period >= 2035,
        0.95,
        value
      ),
      value = if_else(
        code == "EUR" & tech == "TH2F" & period >= 2035,
        0.05,
        value
      ),
      value = if_else(
        region == "IDN" & tech == "TELC" & period == 2050,
        0.95,
        value
      ),
      value = if_else(
        region == "IDN" & tech == "TH2F" & period == 2050,
        0.05,
        value
      ),
      value = if_else(region == "IND", 2 * value, value),
      value = if_else(region == "VNM", 4 * value, value),
      # Add approximate targets for rest of the world
      value = if_else(
        !(code %in% c("CHA", "EUR")) & tech == "TELC" & period >= 2050,
        0.95,
        value
      ),
      value = if_else(
        !(code %in% c("CHA", "EUR")) & tech == "TH2F" & period >= 2050,
        0.05,
        value
      ),
      value = if_else(
        code == "CHA" & tech == "TELC" & period >= 2035,
        0.95,
        value
      ),
      value = if_else(
        code == "CHA" & tech == "TH2F" & period >= 2035,
        0.05,
        value
      ),
      value = ifelse(tech %in% c("TPHEVGDO", "TPHEVGSL") & period >= 2040 & is.na(value), 0, value),
      # Linear interpolation
      value = zoo::na.approx(value, x = period, na.rm = FALSE, rule = 1)
    ) %>%
    ungroup() %>%
    filter(period > 2020) %>%
    select(-code) %>%
    as.quitte() %>%
    as.magpie()

  salesTech <- readSource("IEA_EV", convert = TRUE) %>%
    as.quitte() %>%
    filter(
      variable == "Cars",
      parameter %in% c("EV sales"),
      period == 2020
    ) %>%
    select(c("region", "period", "powertrain", "value")) %>%
    rename(tech = powertrain) %>%
    as.quitte() %>%
    as.magpie()

  TECHS <- toolGetMapping("SECTTECH.csv",
    type = "blabla_export",
    where = "mrprom"
  ) %>%
    separate_rows(c("TECH"), sep = ",") %>%
    separate_rows(c("DSBS"), sep = ",") %>%
    filter(
      DSBS == "PC",
      !(TECH %in% getItems(TsalesTechShares, 3.1))
    ) %>%
    pull(TECH)

  TsalesTechShares <- add_columns(TsalesTechShares,
    addnm = TECHS,
    dim = 3.1,
    fill = NA
  )

  weights <- TsalesTechShares
  salesTechShares <- salesTechShares %>%
    as.quitte() %>%
    as.magpie() %>%
    add_columns(
      addnm = TECHS,
      dim = 3.1,
      fill = NA
    )
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
