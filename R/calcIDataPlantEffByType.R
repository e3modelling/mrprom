#' calcIDataPlantEffByType
#'
#' Use data from IEA and EU Reference Scenario to derive OPENPROM input parameter iDataPlantEffByType
#' This dataset includes plant efficiency per plant type, as a ratio.
#'
#' @return magpie object with OPENPROM input data iDataPlantEffByType.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataPlantEffByType", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select filter rename
#' @importFrom tidyr pivot_wider spread gather complete expand
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIDataPlantEffByType <- function() {
  sectorsELC <- list(
    PG = c("ELMAINE", "ELAUTOE"),
    CHP = c("ELMAINC", "ELAUTOC")
  ) %>%
    stack() %>%
    rename(flow = values, sector = ind)

  sectorsHeat <- list(
    STEAMP = c("HEMAINH", "HEAUTOH"),
    CHP = c("HEMAINC", "HEAUTOC")
  ) %>%
    stack() %>%
    rename(flow = values, sector = ind)

  fuelMap <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, variable = OPEN.PROM)

  dataELC <- readSource("IEA2025", subset = unique(sectorsELC$flow)) %>%
    as.quitte() %>%
    filter(unit == "GWH") %>%
    mutate(
      value = value * 8.598 * 1e-5,
      unit = "Mtoe"
    ) %>%
    select(-variable) %>%
    # map IEA products to OPEN-PROM EFs
    left_join(fuelMap, by = "product") %>%
    left_join(sectorsELC, by = "flow") %>%
    # Aggregate to OPEN-PROM's EFs & SBS
    group_by(region, period, variable, sector) %>%
    summarise(ELC = sum(value, na.rm = TRUE), .groups = "drop")

  dataHeat <- readSource("IEA2025", subset = unique(sectorsHeat$flow)) %>%
    as.quitte() %>%
    filter(unit == "KTOE") %>%
    mutate(
      value = value / 1000,
      unit = "Mtoe"
    ) %>%
    select(-variable) %>%
    # map IEA products to OPEN-PROM EFs
    inner_join(fuelMap, by = "product") %>%
    left_join(sectorsHeat, by = "flow") %>%
    # Aggregate to OPEN-PROM's EFs & SBS
    group_by(region, period, variable, sector) %>%
    summarise(heat = sum(value, na.rm = TRUE), .groups = "drop")

  transfInput <- calcOutput(type = "ITransfProcess", flow = "Inp", aggregate = FALSE) %>%
    as.quitte() %>%
    select(c("region", "period", "sector", "variable", "value")) %>%
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    rename(input = value) %>%
    filter(sector %in% c("PG", "CHP", "STEAMP"))

  PGALLtoEF <- toolGetMapping(
    name = "PGALLtoEF.csv",
    type = "blabla_export",
    where = "mrprom"
  )
  CHPtoEF <- toolGetMapping(
    name = "CHPtoEF.csv",
    type = "blabla_export",
    where = "mrprom"
  ) %>%
    separate_rows(EF, sep = ",")

  DHtoEF <- toolGetMapping(
    name = "DHtoEF.csv",
    type = "blabla_export",
    where = "mrprom"
  ) %>%
    separate_rows(EF, sep = ",")

  map <- left_join(PGALLtoEF, CHPtoEF, by = "EF") %>%
    left_join(DHtoEF, by = "EF") %>%
    rename(variable = EF)

  efficiencies <- dataELC %>%
    full_join(dataHeat, by = c("region", "period", "sector", "variable")) %>%
    inner_join(transfInput, by = c("region", "period", "sector", "variable")) %>%
    inner_join(map, by = "variable", relationship = "many-to-many") %>%
    mutate(
      tech = case_when(
        sector == "PG" ~ as.character(PGALL),
        sector == "CHP" ~ as.character(CHP),
        sector == "STEAMP" ~ as.character(DH),
        TRUE ~ NA
      )
    ) %>%
    pivot_longer(c("ELC", "heat", "input")) %>%
    group_by(region, period, tech, name) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    rename(variable = tech) %>%
    pivot_wider() %>%
    mutate(
      effELC = -ELC / input,
      effHeat = -heat / input
    ) %>%
    imputeGlobal()

  weights <- efficiencies %>%
    mutate(
      effELC = -input + 1e-6,
      effHeat = -input + 1e-6
    ) %>%
    select(-input) %>%
    pivot_longer(c("effELC", "effHeat"), names_to = "eff") %>%
    as.quitte() %>%
    as.magpie()

  efficiencies <- efficiencies %>%
    select(-input) %>%
    pivot_longer(c("effELC", "effHeat"), names_to = "eff") %>%
    # FIXME: NAs must be handled: e.g., HEAT must be distributed to the rest EFs
    as.quitte() %>%
    as.magpie()

  list(
    x = efficiencies,
    weight = weights,
    unit = "Ratio",
    description = "IEA fuel efficiencies"
  )
}

# ------------------------------- Helper ----------------------------------
imputeGlobal <- function(efficiencies) {
  effsGLO <- efficiencies %>%
    pivot_longer(c("ELC", "heat", "input")) %>%
    group_by(period, variable, name) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider() %>%
    mutate(
      effELC = -ELC / input,
      effHeat = -heat / input
    ) %>%
    select(period, variable, effELC, effHeat)

  efficiencies <- efficiencies %>%
    left_join(effsGLO, by = c("period", "variable")) %>%
    mutate(
      effELC = ifelse(is.na(effELC.x) | is.infinite(effELC.x), effELC.y, effELC.x),
      effHeat = ifelse(is.na(effHeat.x) | is.infinite(effHeat.x), effHeat.y, effHeat.x)
    ) %>%
    select(region, period, variable, input, effELC, effHeat)
  return(efficiencies)
}
