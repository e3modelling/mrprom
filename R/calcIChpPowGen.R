#' calcIChpPowGen
#'
#' Use data from EU Reference Scenario to derive OPENPROM input table imDataChpPowGen
#' This dataset includes CHP/DHP economic and technical data initialisation for electricity production.
#' The availability factor is hardcoded and it is based on previous data.
#'
#' @return magpie object with OPENPROM input data iChpPowGen
#'
#' @author Alexandros, Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IChpPowGen", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select filter rename mutate case_when
#' @importFrom tidyr pivot_wider spread gather
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIChpPowGen <- function() {
  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fEndHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndHorizon"]

  mapSteam <- toolGetMapping(
    name = "prom-primes-steam-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    rename(technology = PRIMES)

  mapCHP <- data.frame(
    OPEN.PROM = c(
      "TSTE1AL", "TSTE1AH", "TSTE1AD",
      "TSTE1AG", "TSTE1AB", "TSTE1AH2F"
    ),
    technology = c(
      "Steam Turbine", "Steam Turbine", "Internal Combustion Engine",
      "Gas Turbine", "Steam Turbine", "Fuel Cell"
    )
  )

  map <- bind_rows(mapCHP, mapSteam)

  x <- readSource("TechCosts2024", "PowerAndHeat", convert = TRUE)
  # Rename variable names to have the same variable names as in OPEN-PROM
  getNames(x, 1) <- c("IC", "FC", "VOM", "LFT")
  x <- as.quitte(x) %>%
    mutate(
      # EURO2020 -> USD2015
      value = ifelse(variable %in% c("IC", "FC", "VOM"), value * 1.048, value),
      unit = ifelse(variable %in% c("IC", "FC", "VOM"), "USD2015/kW", unit),
      # USD2015/MWh -> USD2015/toe
      value = ifelse(variable %in% c("VOM"), value * 11.63, value),
      unit = ifelse(variable %in% c("VOM"), "USD2015/toe", unit)
    ) %>%
    select(region, technology, variable, period, value)
  
  # Data from "https://docs.nrel.gov/docs/fy17osti/68579.pdf"
  dataCHP <- data.frame(
    technology = c(
      "Internal Combustion Engine", "Microturbine",
      "Gas Turbine", "Fuel Cell", "Steam Turbine"
    ),
    effElc = c(0.35, 0.25, 0.3, 0.55, 0.08),
    effThrm = c(0.45, 0.4, 0.4, 0.2, 0.72),
    IC = c(2200, 2700, 2400, 8000, 800), # $/kw elec
    VOM = c(15, 13, 12, 40, 8), # $/MWh
    FC = 0,
    LFT = c(20, 20, 20, 20, 20)
  ) %>%
    pivot_longer(cols = -technology, names_to = "variable", values_to = "value") %>%
    mutate(region = "GLO") %>%
    crossing(period = unique(x$period))

  xeff <- readSource("TechCosts2024", "PowerAndHeatEfficiency", convert = TRUE) %>%
    as.quitte() %>%
    rename(technology = variable) %>%
    mutate(variable = "effThrm") %>%
    select(technology, variable, period, value) %>%
    mutate(region = "GLO")

  data <- x %>%
    bind_rows(xeff) %>%
    bind_rows(dataCHP) %>%
    inner_join(
      map,
      by = c("technology"),
      relationship = "many-to-many"
    )

  # FIXME: The plant availability rates are missing from EU Reference Scenario 2020
  df <- data.frame(
    OPEN.PROM = as.character(map[, 1]),
    variable = "AVAIL",
    #period = 2020,
    region = "GLO",
    #unit = "Percentage",
    value = 0.0
  )

  df <- mutate(df, value = case_when(
    OPEN.PROM == "TSTE1AL" ~ 0.8, OPEN.PROM == "TSTE2LGN" ~ 0.85,
    OPEN.PROM == "TSTE1AH" ~ 0.8, OPEN.PROM == "TSTE2OSL" ~ 0.85,
    OPEN.PROM == "TSTE1AD" ~ 0.8, OPEN.PROM == "TSTE2GDO" ~ 0.8,
    OPEN.PROM == "TSTE1AG" ~ 0.93, OPEN.PROM == "TSTE2NGS" ~ 0.8,
    OPEN.PROM == "TSTE1AB" ~ 0.85, OPEN.PROM == "TSTE2BMS" ~ 0.85,
    OPEN.PROM == "TSTE2GEO" ~ 0.9, OPEN.PROM == "TSTE2OTH" ~ 0.9,
    OPEN.PROM == "TSTE1AH2F" ~ 0.97,
    TRUE ~ value
  )) %>%
    crossing(period = unique(data$period))

  xq <- bind_rows(data, df) %>%
    select(-technology) %>%
    as.quitte() %>%
    interpolate_missing_periods(seq(fStartHorizon, fEndHorizon, 1), expand.values = TRUE) %>%
    rename(technology = open.prom) %>%
    as.quitte() %>%
    as.magpie()

  list(
    x = xq,
    weight = NULL,
    unit = "various",
    description = "EU Reference Scenario 2020; Data for power generation (STEAM) costs (various)"
  )
}
