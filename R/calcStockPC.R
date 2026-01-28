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
  fEndY <- readEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fEndY"]

  stockTotalPC <- calcOutput(type = "ACTV", aggregate = FALSE) %>%
    as.quitte() %>%
    filter(
      variable == "PC",
      period <= fEndY,
    ) %>%
    rename(stock = value)

  shareEVs <- readSource("IEA_EV", convert = TRUE) %>%
    as.quitte() %>%
    filter(
      period <= fEndY,
      parameter == "EV stock share",
      variable == "Cars"
    ) %>%
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    select(region, period, powertrain, value) %>%
    rename(tech = powertrain, share = value)

  stockEV <- stockTotalPC %>%
    left_join(shareEVs, by = c("region", "period")) %>%
    mutate(
      stock = stock * share
    ) %>%
    select(region, period, tech, stock)

  stockTotalEV <- stockEV %>%
    group_by(region, period) %>%
    summarise(value = sum(stock, na.rm = TRUE), .groups = "drop")

  shareNonEVs <- helperGetNonEVShares(fEndY)

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
    mutate(
      stock = ifelse(is.na(stock.x), stock.y, stock.x),
      stock = ifelse(stock < 1e-6 | is.na(stock), 0, stock)
    ) %>%
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

# Helpers ---------------------------------------------------------------
#' @export
helperGetEVShares <- function(dataIEA_EV, finalY, cat = "Historical") {
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

helperGetNonEVShares <- function(fEndY) {
  SFC <- calcOutput(type = "ISFC", aggregate = FALSE) %>%
    as.quitte() %>%
    filter(
      period <= fEndY,
      !fuel %in% c("BGSL", "BGDO"),
      !tech %in% c("TELC", "TPHEVGDO", "TPHEVGSL", "TH2F")
    ) %>%
    select(region, period, tech, value) %>%
    rename(SFC = value)

  shareNonEVs <- calcOutput(
    type = "IFuelCons2", subtype = "TRANSE", aggregate = FALSE
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
    filter(
      dsbs == "PC",
      !is.na(value),
      !tech %in% c("TELC", "TPHEVGDO", "TPHEVGSL", "TH2F")
    ) %>%
    right_join(SFC, by = c("region", "period", "tech")) %>%
    mutate(value = replace_na(value, 0) / SFC) %>%
    # Calculate relative % of techs. If no consumption, take uniform
    group_by(region, period) %>%
    mutate(
      share = (value + 1e-6) / (sum(value + 1e-6))
    ) %>%
    ungroup() %>%
    select(region, period, tech, share)
}
