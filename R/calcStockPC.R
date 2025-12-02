#' calcStockPC
#'
#' Derive car stock per fuel technology for all countries.
#'
#' @return magpie object
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' stockPC <- calcOutput("StockPC", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select rename group_by summarise ungroup inner_join full_join right_join recode
#' @importFrom tidyr complete replace_na
#' @importFrom magclass as.magpie
#' @importFrom quitte as.quitte
#'
calcStockPC <- function() {
  dataIEA_EV <- readSource("IEA_EV", convert = TRUE) %>% as.quitte()

  stockTotalPC <- calcOutput(type = "ACTV", aggregate = FALSE) %>%
    as.quitte() %>%
    filter(variable == "PC") %>%
    rename(stock = value)

  shareEVs <- helperGetEVShares(dataIEA_EV)  %>% filter(period <= 2024)
  shareEVs <- shareEVs %>%
    complete(
      region = as.character(unique(stockTotalPC$region)),
      period = as.integer(unique(stockTotalPC$period)),
      tech = as.character(unique(shareEVs$tech)),
      fill = list(share = 0)
    )
  shareNonEVs <- helperGetNonEVShares()

  stockEV <- stockTotalPC %>%
    left_join(shareEVs, by = c("region", "period")) %>%
    mutate(
      stock = stock * share
    ) %>%
    select(region, period, tech, stock)

  stockTotalEV <- stockEV %>%
    group_by(region, period) %>%
    summarise(value = sum(stock, na.rm = TRUE), .groups = "drop")

  stockNonEV <- stockTotalPC %>%
    full_join(stockTotalEV, by = c("region", "period")) %>%
    mutate(stock = stock - value) %>%
    right_join(shareNonEVs, by = c("region", "period")) %>%
    mutate(
      stock = stock * share
    ) %>%
    select(region, period, tech, stock)

  stock <- stockNonEV %>%
    full_join(stockEV, by = c("region", "period", "tech")) %>%
    mutate(stock = ifelse(is.na(stock.x), stock.y, stock.x)) %>%
    select(region, period, tech, stock) %>%
    rename(value = stock) %>%
    replace_na(list(value = 0)) %>%
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

# Helpers ---------------------------------------------------------------
#' @export
helperGetEVShares <- function(dataIEA_EV, cat = "Historical") {
  relativeShareEVs <- dataIEA_EV %>%
    filter(
      parameter == "EV stock",
      category == cat,
      variable == "Cars",
      !is.na(value)
    ) %>%
    # calculate relative % of EVs
    group_by(region, period) %>%
    mutate(
      total_value = sum(value),
      share = value / total_value
    ) %>%
    ungroup() %>%
    select(region, variable, period, share, powertrain)

  # Dissagregate EV stock share to ELC, PHGSL, PHGDO, TH2F
  stockSharesEV <- dataIEA_EV %>%
    filter(
      parameter == "EV stock share",
      category == cat,
      variable == "Cars",
      !is.na(value)
    ) %>%
    right_join(relativeShareEVs, by = c("region", "period"), relationship = "many-to-many") %>%
    mutate(share = value * share / 100) %>%
    select(region, period, powertrain.y, share) %>%
    rename(tech = powertrain.y)
  return(stockSharesEV)
}

helperGetNonEVShares <- function(typeFuelCons = "IFuelCons2", argument = "historical") {
  SFC <- calcOutput(type = "ISFC", aggregate = FALSE, subtype = argument) %>%
    as.quitte() %>%
    filter(!fuel %in% c("BGSL", "BGDO")) %>%
    select(-fuel) %>%
    rename(SFC = value)

  shareNonEVs <- calcOutput(
    type = typeFuelCons, subtype = "TRANSE", aggregate = FALSE
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
      !tech %in% c("TELC", "TPHEVGDO", "TPHEVGSL", "TH2F")
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
