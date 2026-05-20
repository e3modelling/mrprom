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
        code == "LAM" & variable == "PC" & tech == "TELC" & period == 2050,
        0.5,
        value
      ),
      value = ifelse(
        code == "LAM" & variable == "PC" & tech == "TELC" & period == 2100,
        0.7,
        value
      ),
      value = ifelse(
        code == "EUR" & variable == "PC" & tech == "TELC" & period == 2050,
        0.95,
        value
      ),
      value = ifelse(
        code == "EUR" & variable == "PC" & tech == "TELC" & period == 2100,
        0.95,
        value
      ),
      value = ifelse(
        code == "OAS" & variable == "PC" & tech == "TELC" & period == 2050,
        0.65,
        value
      ),
      value = ifelse(
        code == "OAS" & variable == "PC" & tech == "TELC" & period == 2100,
        0.7,
        value
      ),
      value = ifelse(
        code == "SSA" & variable == "PC" & tech == "TELC" & period == 2050,
        0.4,
        value
      ),
      value = ifelse(
        code == "SSA" & variable == "PC" & tech == "TELC" & period == 2100,
        0.55,
        value
      ),
      value = ifelse(
        code == "NEU" & variable == "PC" & tech == "TELC" & period == 2050,
        0.95,
        value
      ),
      value = ifelse(
        code == "NEU" & variable == "PC" & tech == "TELC" & period == 2100,
        0.95,
        value
      ),
      value = ifelse(
        code == "MEA" & variable == "PC" & tech == "TELC" & period == 2050,
        0.3,
        value
      ),
      value = ifelse(
        code == "MEA" & variable == "PC" & tech == "TELC" & period == 2100,
        0.5,
        value
      ),
      value = ifelse(
        code == "REF" & variable == "PC" & tech == "TELC" & period == 2050,
        0.4,
        value
      ),
      value = ifelse(
        code == "REF" & variable == "PC" & tech == "TELC" & period == 2100,
        0.5,
        value
      ),
      value = ifelse(
        code == "CAZ" & variable == "PC" & tech == "TELC" & period == 2050,
        0.6,
        value
      ),
      value = ifelse(
        code == "CAZ" & variable == "PC" & tech == "TELC" & period == 2100,
        0.9,
        value
      ),
      value = ifelse(
        code == "CHA" & variable == "PC" & tech == "TELC" & period == 2050,
        0.9,
        value
      ),
      value = ifelse(
        code == "CHA" & variable == "PC" & tech == "TELC" & period == 2100,
        0.95,
        value
      ),
      value = ifelse(
        code == "GBR" & variable == "PC" & tech == "TELC" & period == 2050,
        0.95,
        value
      ),
      value = ifelse(
        code == "GBR" & variable == "PC" & tech == "TELC" & period == 2100,
        0.95,
        value
      ),
      value = ifelse(
        code == "IND" & variable == "PC" & tech == "TELC" & period == 2050,
        0.5,
        value
      ),
      value = ifelse(
        code == "IND" & variable == "PC" & tech == "TELC" & period == 2100,
        0.75,
        value
      ),
      value = ifelse(
        code == "JPN" & variable == "PC" & tech == "TELC" & period == 2050,
        0.7,
        value
      ),
      value = ifelse(
        code == "JPN" & variable == "PC" & tech == "TELC" & period == 2100,
        0.8,
        value
      ),
      value = ifelse(
        code == "USA" & variable == "PC" & tech == "TELC" & period == 2050,
        0.5,
        value
      ),
      value = ifelse(
        code == "USA" & variable == "PC" & tech == "TELC" & period == 2100,
        0.7,
        value
      ),
      value = ifelse(
        variable == "PC" & tech == "TH2F",
        0.05,
        value
      ),
      
      
      # ---------------------- PB TELC-----------------------------------------
      
      value = ifelse(
        code == "LAM" & variable == "PB" & tech == "TELC" & period == 2050,
        0.5,
        value
      ),
      value = ifelse(
        code == "LAM" & variable == "PB" & tech == "TELC" & period == 2100,
        0.7,
        value
      ),
      value = ifelse(
        code == "EUR" & variable == "PB" & tech == "TELC" & period == 2050,
        0.8,
        value
      ),
      value = ifelse(
        code == "EUR" & variable == "PB" & tech == "TELC" & period == 2100,
        0.95,
        value
      ),
      value = ifelse(
        code == "OAS" & variable == "PB" & tech == "TELC" & period == 2050,
        0.5,
        value
      ),
      value = ifelse(
        code == "OAS" & variable == "PB" & tech == "TELC" & period == 2100,
        0.7,
        value
      ),
      value = ifelse(
        code == "SSA" & variable == "PB" & tech == "TELC" & period == 2050,
        0.4,
        value
      ),
      value = ifelse(
        code == "SSA" & variable == "PB" & tech == "TELC" & period == 2100,
        0.5,
        value
      ),
      value = ifelse(
        code == "NEU" & variable == "PB" & tech == "TELC" & period == 2050,
        0.8,
        value
      ),
      value = ifelse(
        code == "NEU" & variable == "PB" & tech == "TELC" & period == 2100,
        0.9,
        value
      ),
      value = ifelse(
        code == "MEA" & variable == "PB" & tech == "TELC" & period == 2050,
        0.4,
        value
      ),
      value = ifelse(
        code == "MEA" & variable == "PB" & tech == "TELC" & period == 2100,
        0.6,
        value
      ),
      value = ifelse(
        code == "REF" & variable == "PB" & tech == "TELC" & period == 2050,
        0.4,
        value
      ),
      value = ifelse(
        code == "REF" & variable == "PB" & tech == "TELC" & period == 2100,
        0.5,
        value
      ),
      value = ifelse(
        code == "CAZ" & variable == "PB" & tech == "TELC" & period == 2050,
        0.5,
        value
      ),
      value = ifelse(
        code == "CAZ" & variable == "PB" & tech == "TELC" & period == 2100,
        0.6,
        value
      ),
      value = ifelse(
        code == "CHA" & variable == "PB" & tech == "TELC" & period == 2050,
        0.95,
        value
      ),
      value = ifelse(
        code == "CHA" & variable == "PB" & tech == "TELC" & period == 2100,
        0.95,
        value
      ),
      value = ifelse(
        code == "GBR" & variable == "PB" & tech == "TELC" & period == 2050,
        0.8,
        value
      ),
      value = ifelse(
        code == "GBR" & variable == "PB" & tech == "TELC" & period == 2100,
        0.95,
        value
      ),
      value = ifelse(
        code == "IND" & variable == "PB" & tech == "TELC" & period == 2050,
        0.6,
        value
      ),
      value = ifelse(
        code == "IND" & variable == "PB" & tech == "TELC" & period == 2100,
        0.8,
        value
      ),
      value = ifelse(
        code == "JPN" & variable == "PB" & tech == "TELC" & period == 2050,
        0.5,
        value
      ),
      value = ifelse(
        code == "JPN" & variable == "PB" & tech == "TELC" & period == 2100,
        0.8,
        value
      ),
      value = ifelse(
        code == "USA" & variable == "PB" & tech == "TELC" & period == 2050,
        0.5,
        value
      ),
      value = ifelse(
        code == "USA" & variable == "PB" & tech == "TELC" & period == 2100,
        0.7,
        value
      ),
      
      #############
      # ---------------------- PB TH2F-----------------------------------------
      
      value = ifelse(
        code == "LAM" & variable == "PB" & tech == "TH2F" & period == 2050,
        0.028,
        value
      ),
      value = ifelse(
        code == "LAM" & variable == "PB" & tech == "TH2F" & period == 2100,
        0.05,
        value
      ),
      value = ifelse(
        code == "EUR" & variable == "PB" & tech == "TH2F" & period == 2050,
        0.05,
        value
      ),
      value = ifelse(
        code == "EUR" & variable == "PB" & tech == "TH2F" & period == 2100,
        0.05,
        value
      ),
      value = ifelse(
        code == "OAS" & variable == "PB" & tech == "TH2F" & period == 2050,
        0.028,
        value
      ),
      value = ifelse(
        code == "OAS" & variable == "PB" & tech == "TH2F" & period == 2100,
        0.05,
        value
      ),
      value = ifelse(
        code == "SSA" & variable == "PB" & tech == "TH2F" & period == 2050,
        0.028,
        value
      ),
      value = ifelse(
        code == "SSA" & variable == "PB" & tech == "TH2F" & period == 2100,
        0.05,
        value
      ),
      value = ifelse(
        code == "NEU" & variable == "PB" & tech == "TH2F" & period == 2050,
        0.029,
        value
      ),
      value = ifelse(
        code == "NEU" & variable == "PB" & tech == "TH2F" & period == 2100,
        0.05,
        value
      ),
      value = ifelse(
        code == "MEA" & variable == "PB" & tech == "TH2F" & period == 2050,
        0.028,
        value
      ),
      value = ifelse(
        code == "MEA" & variable == "PB" & tech == "TH2F" & period == 2100,
        0.05,
        value
      ),
      value = ifelse(
        code == "REF" & variable == "PB" & tech == "TH2F" & period == 2050,
        0.028,
        value
      ),
      value = ifelse(
        code == "REF" & variable == "PB" & tech == "TH2F" & period == 2100,
        0.05,
        value
      ),
      value = ifelse(
        code == "CAZ" & variable == "PB" & tech == "TH2F" & period == 2050,
        0.028,
        value
      ),
      value = ifelse(
        code == "CAZ" & variable == "PB" & tech == "TH2F" & period == 2100,
        0.05,
        value
      ),
      value = ifelse(
        code == "CHA" & variable == "PB" & tech == "TH2F" & period == 2050,
        0.05,
        value
      ),
      value = ifelse(
        code == "CHA" & variable == "PB" & tech == "TH2F" & period == 2100,
        0.05,
        value
      ),
      value = ifelse(
        code == "GBR" & variable == "PB" & tech == "TH2F" & period == 2050,
        0.032,
        value
      ),
      value = ifelse(
        code == "GBR" & variable == "PB" & tech == "TH2F" & period == 2100,
        0.05,
        value
      ),
      value = ifelse(
        code == "IND" & variable == "PB" & tech == "TH2F" & period == 2050,
        0.029,
        value
      ),
      value = ifelse(
        code == "IND" & variable == "PB" & tech == "TH2F" & period == 2100,
        0.05,
        value
      ),
      value = ifelse(
        code == "JPN" & variable == "PB" & tech == "TH2F" & period == 2050,
        0.028,
        value
      ),
      value = ifelse(
        code == "JPN" & variable == "PB" & tech == "TH2F" & period == 2100,
        0.1,
        value
      ),
      value = ifelse(
        code == "USA" & variable == "PB" & tech == "TH2F" & period == 2050,
        0.034,
        value
      ),
      value = ifelse(
        code == "USA" & variable == "PB" & tech == "TH2F" & period == 2100,
        0.05,
        value
      ),
      
      # ---------------------- GU -----------------------------------------

      # ---------------------- GU TELC-----------------------------------------
      
      value = ifelse(
        code == "LAM" & variable == "GU" & tech == "TELC" & period == 2050,
        0.05,
        value
      ),
      value = ifelse(
        code == "LAM" & variable == "GU" & tech == "TELC" & period == 2100,
        0.4,
        value
      ),
      value = ifelse(
        code == "EUR" & variable == "GU" & tech == "TELC" & period == 2050,
        0.6662,
        value
      ),
      value = ifelse(
        code == "EUR" & variable == "GU" & tech == "TELC" & period == 2100,
        0.6662,
        value
      ),
      value = ifelse(
        code == "OAS" & variable == "GU" & tech == "TELC" & period == 2050,
        0.15,
        value
      ),
      value = ifelse(
        code == "OAS" & variable == "GU" & tech == "TELC" & period == 2100,
        0.6,
        value
      ),
      value = ifelse(
        code == "SSA" & variable == "GU" & tech == "TELC" & period == 2050,
        0.05,
        value
      ),
      value = ifelse(
        code == "SSA" & variable == "GU" & tech == "TELC" & period == 2100,
        0.3,
        value
      ),
      value = ifelse(
        code == "NEU" & variable == "GU" & tech == "TELC" & period == 2050,
        0.64,
        value
      ),
      value = ifelse(
        code == "NEU" & variable == "GU" & tech == "TELC" & period == 2100,
        0.8,
        value
      ),
      value = ifelse(
        code == "MEA" & variable == "GU" & tech == "TELC" & period == 2050,
        0.05,
        value
      ),
      value = ifelse(
        code == "MEA" & variable == "GU" & tech == "TELC" & period == 2100,
        0.25,
        value
      ),
      value = ifelse(
        code == "REF" & variable == "GU" & tech == "TELC" & period == 2050,
        0.05,
        value
      ),
      value = ifelse(
        code == "REF" & variable == "GU" & tech == "TELC" & period == 2100,
        0.25,
        value
      ),
      value = ifelse(
        code == "CAZ" & variable == "GU" & tech == "TELC" & period == 2050,
        0.25,
        value
      ),
      value = ifelse(
        code == "CAZ" & variable == "GU" & tech == "TELC" & period == 2100,
        0.5,
        value
      ),
      value = ifelse(
        code == "CHA" & variable == "GU" & tech == "TELC" & period == 2050,
        0.66,
        value
      ),
      value = ifelse(
        code == "CHA" & variable == "GU" & tech == "TELC" & period == 2100,
        0.7,
        value
      ),
      value = ifelse(
        code == "GBR" & variable == "GU" & tech == "TELC" & period == 2050,
        0.6662,
        value
      ),
      value = ifelse(
        code == "GBR" & variable == "GU" & tech == "TELC" & period == 2100,
        0.6662,
        value
      ),
      value = ifelse(
        code == "IND" & variable == "GU" & tech == "TELC" & period == 2050,
        0.1,
        value
      ),
      value = ifelse(
        code == "IND" & variable == "GU" & tech == "TELC" & period == 2100,
        0.4,
        value
      ),
      value = ifelse(
        code == "JPN" & variable == "GU" & tech == "TELC" & period == 2050,
        0.2,
        value
      ),
      value = ifelse(
        code == "JPN" & variable == "GU" & tech == "TELC" & period == 2100,
        0.5,
        value
      ),
      value = ifelse(
        code == "USA" & variable == "GU" & tech == "TELC" & period == 2050,
        0.1888,
        value
      ),
      value = ifelse(
        code == "USA" & variable == "GU" & tech == "TELC" & period == 2100,
        0.5,
        value
      ),
      
      #############
      # ---------------------- GU TH2F-----------------------------------------
      
      value = ifelse(
        code == "LAM" & variable == "GU" & tech == "TH2F" & period == 2050,
        0.000688438,
        value
      ),
      value = ifelse(
        code == "LAM" & variable == "GU" & tech == "TH2F" & period == 2100,
        0.000688438,
        value
      ),
      value = ifelse(
        code == "EUR" & variable == "GU" & tech == "TH2F" & period == 2050,
        0.021523132,
        value
      ),
      value = ifelse(
        code == "EUR" & variable == "GU" & tech == "TH2F" & period == 2100,
        0.021523132,
        value
      ),
      value = ifelse(
        code == "OAS" & variable == "GU" & tech == "TH2F" & period == 2050,
        0.000688438,
        value
      ),
      value = ifelse(
        code == "OAS" & variable == "GU" & tech == "TH2F" & period == 2100,
        0.000688438,
        value
      ),
      value = ifelse(
        code == "SSA" & variable == "GU" & tech == "TH2F" & period == 2050,
        0.000688438,
        value
      ),
      value = ifelse(
        code == "SSA" & variable == "GU" & tech == "TH2F" & period == 2100,
        0.000688438,
        value
      ),
      value = ifelse(
        code == "NEU" & variable == "GU" & tech == "TH2F" & period == 2050,
        0.000688438,
        value
      ),
      value = ifelse(
        code == "NEU" & variable == "GU" & tech == "TH2F" & period == 2100,
        0.000688438,
        value
      ),
      value = ifelse(
        code == "MEA" & variable == "GU" & tech == "TH2F" & period == 2050,
        0.000688438,
        value
      ),
      value = ifelse(
        code == "MEA" & variable == "GU" & tech == "TH2F" & period == 2100,
        0.000688438,
        value
      ),
      value = ifelse(
        code == "REF" & variable == "GU" & tech == "TH2F" & period == 2050,
        0.000688438,
        value
      ),
      value = ifelse(
        code == "REF" & variable == "GU" & tech == "TH2F" & period == 2100,
        0.000688438,
        value
      ),
      value = ifelse(
        code == "CAZ" & variable == "GU" & tech == "TH2F" & period == 2050,
        0.000688438,
        value
      ),
      value = ifelse(
        code == "CAZ" & variable == "GU" & tech == "TH2F" & period == 2100,
        0.000688438,
        value
      ),
      value = ifelse(
        code == "CHA" & variable == "GU" & tech == "TH2F" & period == 2050,
        0.029114126,
        value
      ),
      value = ifelse(
        code == "CHA" & variable == "GU" & tech == "TH2F" & period == 2100,
        0.029114126,
        value
      ),
      value = ifelse(
        code == "GBR" & variable == "GU" & tech == "TH2F" & period == 2050,
        0.021523132,
        value
      ),
      value = ifelse(
        code == "GBR" & variable == "GU" & tech == "TH2F" & period == 2100,
        0.021523132,
        value
      ),
      value = ifelse(
        code == "IND" & variable == "GU" & tech == "TH2F" & period == 2050,
        0.002937853,
        value
      ),
      value = ifelse(
        code == "IND" & variable == "GU" & tech == "TH2F" & period == 2100,
        0.002937853,
        value
      ),
      value = ifelse(
        code == "JPN" & variable == "GU" & tech == "TH2F" & period == 2050,
        0.1,
        value
      ),
      value = ifelse(
        code == "JPN" & variable == "GU" & tech == "TH2F" & period == 2100,
        0.036548223,
        value
      ),
      value = ifelse(
        code == "USA" & variable == "GU" & tech == "TH2F" & period == 2050,
        0.036548223,
        value
      ),
      value = ifelse(
        code == "USA" & variable == "GU" & tech == "TH2F" & period == 2100,
        0.05,
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
    mixed_aggregation = TRUE
    # aggregationArguments = list(zeroWeight = "setNA")
  )
}
