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
#' @importFrom tidyr complete replace_na recode
#' @importFrom magclass as.magpie
#' @importFrom quitte as.quitte
#' @importFrom zoo na.locf
#'
calcTNewShareStockPC <- function() {
  # -------------- IEA ELC share sales -----------
  salesTechShares <- readSource("IEA_EV", convert = TRUE) %>%
    as.quitte() %>%
    filter(
      variable %in% c("Cars", "Buses", "Trucks"),
      parameter %in% c("EV sales share")
    ) %>%
    select(c("region", "period", "variable", "powertrain", "value")) %>%
    rename(tech = powertrain) %>%
    mutate(variable = recode(variable,
      "Cars" = "PC",
      "Buses" = "PB",
      "Trucks" = "GU"
    ))


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
    group_by(region, variable, tech, code) %>%
    complete(period = 2021:2100) %>%
    # Extend targets after 2030
    # ---------------------- PC -----------------------------------------
    mutate(
      value = ifelse(
        region == "BRA" & variable == "PC",
        3 * value,
        value
      ),
      value = ifelse(region == "CAN" & variable == "PC", 4 * value, value),
      value = ifelse(
        region == "CAN" & tech == "TELC" & period == 2035 & variable == "PC",
        0.95,
        value
      ),
      value = ifelse(
        region == "CAN" & tech == "TH2F" & period == 2035 & variable == "PC",
        0.05,
        value
      ),
      value = ifelse(
        code == "EUR" & tech == "TELC" & period >= 2035 & variable == "PC",
        0.95,
        value
      ),
      value = ifelse(
        code == "EUR" & tech == "TH2F" & period >= 2035 & variable == "PC",
        0.05,
        value
      ),
      value = ifelse(
        region == "IDN" & tech == "TELC" & period == 2050 & variable == "PC",
        0.95,
        value
      ),
      value = ifelse(
        region == "IDN" & tech == "TH2F" & period == 2050 & variable == "PC",
        0.05,
        value
      ),
      value = ifelse(region == "IND" & variable == "PC", 2 * value, value),
      value = ifelse(region == "VNM" & variable == "PC", 4 * value, value),
      # Add approximate targets for rest of the world
      value = ifelse(
        !(code %in% c("CHA", "EUR")) & tech == "TELC" & period >= 2050 & variable == "PC",
        0.95,
        value
      ),
      value = ifelse(
        !(code %in% c("CHA", "EUR")) & tech == "TH2F" & period >= 2050 & variable == "PC",
        0.05,
        value
      ),
      value = ifelse(
        code == "CHA" & tech == "TELC" & period >= 2035 & variable == "PC",
        0.95,
        value
      ),
      value = ifelse(
        code == "CHA" & tech == "TH2F" & period >= 2035 & variable == "PC",
        0.05,
        value
      ),
      # ---------------------- PB -----------------------------------------
      value = ifelse(
        code == "CHA" & variable == "PB" & period >= 2040 & tech == "TELC",
        0.95,
        value
      ),
      value = ifelse(
        code == "CHA" & variable == "PB" & period >= 2040 & tech == "TH2F",
        0.05,
        value
      ),
      value = ifelse(
        code == "EUR" & variable == "PB" & period >= 2050 & tech == "TELC",
        0.95,
        value
      ),
      value = ifelse(
        code == "EUR" & variable == "PB" & period >= 2050 & tech == "TH2F",
        0.05,
        value
      ),
      value = ifelse(
        !(code %in% c("CHA", "EUR")) & variable == "PB" & period >= 2070 & tech == "TELC",
        0.95,
        value
      ),
      value = ifelse(
        !(code %in% c("CHA", "EUR")) & variable == "PB" & period >= 2070 & tech == "TH2F",
        0.05,
        value
      ),
      # ---------------------- GU -----------------------------------------
      value = ifelse(
        variable == "GU" & period >= 2050,
        3 * value[period == 2030],
        value
      ),
      # ----------------------------------------------------------------------
      value = ifelse(tech %in% c("TPHEVGDO", "TPHEVGSL") & period >= 2040 & is.na(value), 0, value),
      # ---------------------- Interpolate -----------------------------
      # Linear interpolation
      value = zoo::na.approx(value, x = period, na.rm = FALSE, rule = 1)
    ) %>%
    ungroup() %>%
    filter(period > 2020) %>%
    select(-code) %>%
    as.quitte() %>%
    as.magpie()

  SECTTECH <- toolGetMapping("SECTTECH.csv",
    type = "blabla_export",
    where = "mrprom"
  ) %>%
    separate_rows(c("TECH"), sep = ",") %>%
    separate_rows(c("DSBS"), sep = ",") %>%
    filter(
      DSBS %in% getItems(TsalesTechShares, 3.1),
      !(TECH %in% getItems(TsalesTechShares, 3.2))
    )

  TsalesTechShares <- add_columns(TsalesTechShares,
    addnm = paste(SECTTECH$DSBS, SECTTECH$TECH, sep = "."),
    dim = 3,
    fill = NA
  )

  # ---------- Aggregation weights -----------------
  salesTech <- readSource("IEA_EV", convert = TRUE) %>%
    as.quitte() %>%
    filter(
      variable %in% c("Cars", "Buses", "Trucks"),
      parameter %in% c("EV sales")
    ) %>%
    select(c("region", "period", "variable", "powertrain", "value")) %>%
    rename(tech = powertrain) %>%
    mutate(
      variable = recode(variable,
      "Cars" = "PC",
      "Buses" = "PB",
      "Trucks" = "GU"
    ),
    #value = ifelse(is.finite(value), value, 0)
    ) %>%
    as.quitte() %>%
    as.magpie() %>%
    add_columns(
      addnm = paste(SECTTECH$DSBS, SECTTECH$TECH, sep = "."),
      dim = 3,
      fill = 0
    )

  weights <- TsalesTechShares
  weights[, , ] <- salesTech[, "y2020", ] + 1e-6
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
