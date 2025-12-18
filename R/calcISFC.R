#' calcISFC
#'
#' Derive Specific fuel consumption per car technology for all countries
#'
#' @return magpie object
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' SFC <- calcOutput("ISFC", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>%  mutate rename select group_by summarise ungroup inner_join left_join rowwise bind_rows reframe
#' @importFrom tidyr replace_na
#' @importFrom magclass as.magpie
#' @importFrom quitte as.quitte

calcISFC <- function(subtype = "historical") {
  mappingTechnologies <- data.frame(
    category = c(
      "Internal combustion engine",
      "Internal combustion engine", "Internal combustion engine",
      "Internal combustion engine", "Internal combustion engine",
      "Conv_ Hybrid", "Conv_ Hybrid",
      "Plug In Hybrid", "Plug In Hybrid", "Plug In Hybrid",
      "Pure Electric Vehicles", "Fuel cells and other"
    ),
    fuel = c(
      "LPG", "Gasoline", "Diesel oil",
      "Natural gas", "Ethanol", "Gasoline", "Diesel oil",
      "Gasoline", "Diesel oil", "Electricity",
      "Pure Electric Vehicles", "Hydrogen"
    ),
    code = c(
      "TLPG", "TGSL", "TGDO", "TNGS",
      "TETH", "TCHEVGSL", "TCHEVGDO",
      "TPHEVGSL", "TPHEVGDO", "TPHEVELC",
      "TELC", "TH2F"
    ),
    stringsAsFactors = FALSE
  )
  mappingTechnologiesToFuels <- mappingTechnologies %>%
    select(code) %>%
    mutate(
      fuel = code,
      fuel = case_when(
        code == "TPHEVGSL" ~ "GSL",
        code == "TPHEVGDO" ~ "GDO",
        code == "TPHEVELC" ~ "ELC",
        code == "TCHEVGSL" ~ "GSL",
        code == "TCHEVGDO" ~ "GDO",
        TRUE ~ sub("^T", "", code)
      )
    ) %>%
    bind_rows(
      tibble(code = "TGDO", fuel = "BGDO"),
      tibble(code = "TGSL", fuel = "BGSL")
    ) %>%
    rename(tech = code)

  SFC <- helpGetHistoricalSFC(mappingTechnologies) %>%
    helperCorrectSFC()

  if (subtype == "projection") {
    SFCProjectedEvolEU <- helperGetProjSFCEU(mappingTechnologies)

    SFC <- SFC %>%
      filter(period == 2020) %>%
      select(-period) %>%
      inner_join(SFCProjectedEvolEU, by = c("tech"), relationship = "many-to-many") %>%
      mutate(value = value * ratio) %>%
      select(-ratio) %>%
      bind_rows(SFC) %>%
      arrange(period)
  }

  # Transfer PHEVELC from technologies to fuel mode of all plug-ins
  # Add a fuel a column
  SFC <- SFC %>%
    left_join(mappingTechnologiesToFuels, by = "tech", relationship = "many-to-many")

  tempPHEVELC <- SFC %>%
    filter(tech == "TPHEVELC") %>%
    rowwise() %>%
    reframe(
      region = region,
      period = period,
      value = value,
      tech = c("TPHEVGSL", "TPHEVGDO"),
      fuel = "ELC"
    )

  SFC <- SFC %>%
    filter(tech != "TPHEVELC") %>%
    bind_rows(tempPHEVELC) %>%
    as.quitte() %>%
    as.magpie()

  list(
    x = SFC,
    weight = NULL,
    unit = "toe per vkm",
    description = "Primes; Specific fuel consumption"
  )
}

# Helpers ------------------------------------------------------------------
helpGetHistoricalSFC <- function(mappingTechnologies) {
  suppressWarnings({
    stockPC <- calcOutput(type = "ACTV", aggregate = FALSE) %>%
      as.quitte() %>%
      filter(variable == "PC", !is.na(value), value != 0) %>%
      rename(stock = value)
  })

  # European SFCs from Primes
  SFCEU <- readSource("PrimesNewTransport", subtype = "Indicators") %>%
    as.quitte() %>%
    interpolate_missing_periods(period = 2010:2100, expand.values = TRUE) %>%
    filter(period >= 2010, sector == "PC") %>%
    inner_join(mappingTechnologies, by = c("category", "fuel"), relationship = "many-to-many") %>%
    rename(tech = code) %>%
    select(c("region", "period", "unit", "tech", "value")) %>%
    mutate(unit = "ktoe/Gvkm")

  regionsEU <- unique(SFCEU$region)

  # Average SFC for EU countries
  meanSFCEU <- SFCEU %>%
    group_by(period, tech) %>%
    summarise(meanSFC = mean(value, na.rm = TRUE), .groups = "drop")

  # Calculate total Final energy per country and scale it with car stock and mean km
  estimateSFCGlobal <- calcOutput(type = "IFuelCons2", subtype = "TRANSE", aggregate = FALSE) %>%
    as.quitte() %>%
    select(-variable) %>%
    # ---------------------------------------------------
    # Merge efs used as a mix (GSL+BGSL -> GSL, GDO+BGDO -> GDO)
    mutate(
      ef = ifelse(ef == "BGSL", "GSL", as.character(ef)),
      ef = ifelse(ef == "BGDO", "GDO", as.character(ef)),
      ef = factor(ef)
    ) %>%
    group_by(across(-value)) %>% # group by all columns except 'value'
    summarise(value = sum(value), .groups = "drop") %>%
    # ---------------------------------------------------
    rename(tech = ef, variable = dsbs) %>%
    filter(variable == "PC") %>%
    group_by(region, period) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    left_join(stockPC, by = c("region", "period")) %>%
    mutate(value = value / stock / 15000) %>%
    select(region, period, value)

  # Estimated general SFC for EU countries
  estimateSFCEU <- estimateSFCGlobal %>%
    filter(region %in% regionsEU) %>%
    group_by(period) %>%
    summarise(meanSFCEU = mean(value, na.rm = TRUE)) %>%
    interpolate_missing_periods(
      value = "meanSFCEU",
      period = unique(estimateSFCGlobal$period),
      expand.values = TRUE
    )

  # Estimate SFC per tech by scaling the european SFC by the ratio of aggregate SFCs
  SFC <- estimateSFCGlobal %>%
    left_join(meanSFCEU, by = "period", relationship = "many-to-many") %>%
    left_join(estimateSFCEU, by = c("period")) %>%
    mutate(value = meanSFC * value / meanSFCEU) %>%
    left_join(SFCEU, by = c("region", "period", "tech")) %>%
    mutate(
      value = ifelse(!is.na(value.y), value.y, value.x),
      unit = "toe per vkm"
    ) %>%
    select(region, period, tech, unit, value)
  return(SFC)
}

helperCorrectSFC <- function(SFC) {
  baselineCountry <- "AUT"

  correctedSFC <- filter(SFC, !is.na(value))
  baselineSFC <- correctedSFC %>%
    filter(region == baselineCountry) %>%
    select(-c("region", "unit")) %>%
    rename(base = value)

  # keep entries that are less than 50% divergent from baseline country
  # For cars of new technologies, keep baseline SFC
  newTechs <- c(
    "ETH", "ELC", "H2F"
  )
  correctedSFC <- correctedSFC %>%
    left_join(baselineSFC, by = c("period", "tech")) %>%
    filter(abs(value / base - 1) < 0.9) %>%
    mutate(value = ifelse(tech %in% newTechs, base, value)) %>%
    #filter(period == 2020) %>%
    select(-base) %>%
    as.quitte() %>%
    as.magpie()

  # Now apply toolCountryFill if needed for other types of gaps
  suppressMessages(
    suppressWarnings(
      temp <- toolCountryFill(correctedSFC) %>%
        as.quitte()
    )
  )

  # Fill NAs with baseline country
  correctedSFC <- temp %>%
    left_join(
      filter(temp, region == baselineCountry) %>% select(-region),
      by = c("period", "tech")
    ) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(region, period, tech, value)
}

helperGetProjSFCEU <- function(mappingTechnologies) {
  # European SFCs from Primes
  SFCProjectedEvolEU <- readSource("PrimesNewTransport", subtype = "Indicators") %>%
    as.quitte() %>%
    interpolate_missing_periods(period = 2015:2100, expand.values = TRUE) %>%
    filter(period >= 2020, sector == "PC") %>%
    inner_join(mappingTechnologies, by = c("category", "fuel"), relationship = "many-to-many") %>%
    rename(tech = code) %>%
    select(c("region", "period", "unit", "tech", "value")) %>%
    group_by(period, tech) %>%
    summarise(meanSFC = mean(value, na.rm = TRUE), .groups = "drop") %>%
    group_by(tech) %>%
    mutate(
      baseSFC = meanSFC[period == 2020],
      ratio = meanSFC / baseSFC
    ) %>%
    ungroup() %>%
    filter(period >= 2021) %>%
    select(period, tech, ratio)
}

helperEstimateOtherPassModes <- function() {
  df <- data.frame(
    mode = c("PT", "PT", "PT", "PT", "PA", "GU", "GU", "GU", "GU", "GU", "GU", "GU", "GU", "GU", "GU", "GU", "GU", "GU", "GU", "GT", "GT", "GT", "GT", "GN", "GN", "GN"),
    tech = c("GDO", "MET", "H2F", "ELC", "H2F", "LPG", "GSL", "GDO", "NGS", "MET", "ETH", "BGDO", "H2F", "ELC", "PHEVGSL", "PHEVGSL", "PHEVGDO", "PHEVGDO", "CHEVGDO", "GDO", "MET", "H2F", "ELC", "GSL", "GDO", "H2F"),
    fuel = c("GDO", "MET", "H2F", "ELC", "H2F", "LPG", "GSL", "GDO", "NGS", "MET", "ETH", "BGDO", "H2F", "ELC", "GSL", "ELC", "GDO", "ELC", "GDO", "GDO", "MET", "H2F", "ELC", "GSL", "GDO", "H2F"),
    value = c(18.6313, 12.6, 8.9, 2.73638, 21.7, 54.1073, 60.1192, 45.0894, 66, 56.2, 80, 45.0894, 13.5268, 27.0536, 34.4, 21.8, 27.0536, 21.8, 21.8, 33.629, 78, 92, 11.5245, 22.8, 15.2, 8.14286)
  )
}
