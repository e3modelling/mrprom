#' calcTStockPC
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
#' TStockPC <- calcOutput("TStockPC", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select rename group_by summarise ungroup inner_join full_join right_join recode
#' @importFrom tidyr expand_grid replace_na
#' @importFrom magclass as.magpie
#' @importFrom quitte as.quitte
#'
calcTStockPC <- function(subtype = 2021) {
  
  finalY <- subtype
  
  mappingEVs <- list(
    "BEV" = "TELC",
    "PHEVGDO" = "TPHEVGDO",
    "PHEVGSL" = "TPHEVGSL",
    "FCEV" = "TH2F"
  )
  
  technologyMapping <- list(
    "CNG" = "TNGS",
    "Diesel Conventional" = "TGDO",
    "Diesel Hybrid" = "TCHEVGDO",
    "Diesel plug-in hybrid" = "TPHEVGDO",
    "E85" = "TETH",
    "Electric" = "TELC",
    "Gasoline Conventional" = "TGSL",
    "Gasoline Hybrid" = "TCHEVGSL",
    "Gasoline plug-in hybrid" = "TPHEVGSL",
    "Hydrogen" = "TH2F",
    "LPG" = "TLPG"
  )
  
  stockEU <- readSource("PrimesNewTransport", subtype = "Stock") %>%
    as.quitte() %>%
    filter(period >= 2010, sector == "PC") %>%
    mutate(
      variable = recode(variable, !!!technologyMapping),
      value = value / 1e3,
      unit = "million vehicles"
    )
  
  SFC <- calcOutput(type = "ISFC", subtype = "projection", aggregate = FALSE) %>%
    as.quitte() %>%
    filter(!fuel %in% c("BGSL", "BGDO")) %>%
    select(-fuel) %>%
    rename(SFC = value)
  
  ACTV <- calcOutput(type = "ACTV", aggregate = FALSE)
  
  carStockTotal <- ACTV %>%
    as.quitte() %>%
    interpolate_missing_periods(period = getYears(ACTV, as.integer = TRUE), expand.values = TRUE) %>%
    filter(variable == "PC") %>%
    rename(stock = value)
  
  dataIEA_EV <- readSource("IEA_EV", convert = TRUE) %>% as.quitte()
  shareEVs <- helperGetEVSharesTargets(mappingEVs, dataIEA_EV, finalY)
  shareNonEVs <- helperGetNonEVSharesT(SFC, mappingEVs)
  
  stockEV <- carStockTotal %>%
    inner_join(shareEVs, by = c("region", "period")) %>%
    mutate(
      stock = stock * share
    ) %>%
    select(region, period, tech, stock)
  
  stockTotalEV <- stockEV %>%
    group_by(region, period) %>%
    summarise(value = sum(stock), .groups = "drop")
  
  stockNonEV <- carStockTotal %>%
    inner_join(stockTotalEV, by = c("region", "period")) %>%
    mutate(stock = stock - value) %>%
    right_join(shareNonEVs, by = c("region", "period")) %>%
    mutate(
      stock = stock * share
    ) %>%
    select(region, period, tech, stock)
  
  stock <- stockNonEV %>%
    full_join(stockEV, by = c("region", "period", "tech")) %>%
    filter(period > 2020) %>%
    mutate(stock = ifelse(is.na(stock.x), stock.y, stock.x)) %>%
    select(region, period, tech, stock) %>%
    rename(value = stock) %>%
    as.quitte() %>%
    as.magpie()
  
  stock[is.na(stock)] <- 0
  
  list(
    x = stock,
    weight = NULL,
    unit = "million vehicles",
    description = "Activity data for OPENPROM sectors"
  )
}

# -------------------------------------------------------------------
#' @export
helperGetEVSharesTargets <- function(mappingEVs, dataIEA_EV, finalY, fillRegions = TRUE) {
  cat <- "Historical"
  if (finalY >= 2021) cat <- "Projection-STEPS"
  
  sharesEVTechs <- dataIEA_EV %>%
    filter(
      parameter == "EV stock",
      category == cat,
      variable == "Cars",
      !is.na(value),
      period >= finalY
    ) %>%
    # Split PHEV into PHEVGSL and PHEVGDO
    mutate(
      value = if_else(powertrain == "PHEV", value * 0.5, value),
      powertrain = case_when(
        powertrain == "PHEV" ~ "TPHEVGSL",
        TRUE ~ powertrain
      )
    )
  
  phevgdo <- sharesEVTechs %>%
    filter(powertrain == "TPHEVGSL") %>%
    mutate(
      powertrain = case_when(
        powertrain == "TPHEVGSL" ~ "TPHEVGDO",
        TRUE ~ powertrain
      )
    )
  
  # Add PHEVGDO (equal to PHEVGSL) and calculate shares between EVs
  sharesEVTechs <- sharesEVTechs %>%
    bind_rows(phevgdo) %>%
    # calculate relative % of EVs
    group_by(region, period) %>%
    mutate(
      total_value = sum(value),
      share = value / total_value
    ) %>%
    ungroup() %>%
    select(region, variable, period, share, powertrain) %>%
    mutate(powertrain = recode(powertrain, !!!mappingEVs))
  
  stockSharesEV <- dataIEA_EV %>%
    filter(
      parameter == "EV stock share",
      category == cat,
      variable == "Cars",
      !is.na(value),
      period >= finalY
    ) %>%
    right_join(sharesEVTechs, by = c("region", "period"), relationship = "many-to-many") %>%
    mutate(share = value * share / 100) %>%
    select(region, period, powertrain.y, share) %>%
    rename(tech = powertrain.y)
  
  if (fillRegions == TRUE) {
    # Fill rest of countries with share 0
    stockSharesEV <- expand_grid(
      region = unname(getISOlist()),
      period = unique(stockSharesEV$period),
      tech = unique(stockSharesEV$tech)
    ) %>%
      left_join(stockSharesEV, by = c("region", "period", "tech")) %>%
      mutate(share = replace_na(share, 0))
  }
  return(stockSharesEV)
}

helperGetNonEVSharesT <- function(SFC, mappingEVs) {
  shareNonEVs <- calcOutput(
    type = "IFuelCons", subtype = "TRANSE", aggregate = FALSE
  ) %>%
    as.quitte() %>%
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
  rename(tech = ef) %>%
    mutate(tech = paste0("T", tech)) %>%
    filter(dsbs == "PC") %>%
    right_join(SFC, by = c("region", "period", "tech")) %>%
    mutate(value = replace_na(value, 0) / SFC) %>%
    filter(
      !is.na(value),
      !tech %in% unlist(mappingEVs, use.names = F)
    ) %>%
    # Calculate relative % of techs
    group_by(region, period) %>%
    mutate(
      total_value = sum(value),
      share = value / total_value
    ) %>%
    ungroup() %>%
    select(region, period, tech, share)
}
