#' calcITransfProcess
#'
#' Construct transformation process energy balances using IEA energy system data
#' and map them to OPEN-PROM transformation sectors.
#'
#' This function derives the OPEN-PROM input variable ITransfProcess, which
#' represents energy flows in transformation processes (e.g. power generation,
#' CHP, district heating) expressed in Mtoe.
#'
#' The IEA dataset covers approximately 160 countries and 29 regions up to 2023.
#' To ensure consistency with the OPEN-PROM 249-country structure, all missing
#' countries are added and filled with zero values.
#'
#' The model distinguishes between three transformation modes:
#'
#' {Total}: all transformation flows included
#' {CHP}: combined heat and power only
#' {DHP}: district heating plants only
#'
#' In addition, two flow directions are considered:
#'
#' {Inp}: input flows (negative values in IEA balance)
#' {Out}: output flows (positive values)
#'
#' The filtering rule is applied as:
#'
#' {Inp: } value < 0 
#' {Out: } value > 0
#' 
#' IEA products are mapped to OPEN-PROM fuel variables using
#' {prom-iea-fuelcons-mapping.csv}, and transformation flows are
#' further mapped to OPEN-PROM sectors using
#' {iea-transfProcess-mapping.csv}.
#'
#' Data are converted from kilotonnes of oil equivalent (KTOE) to Mtoe
#' by dividing by 1000, then aggregated by region, year, variable, and sector.
#'
#' Missing values are filled with zeros using {toolCountryFill()},
#' followed by explicit NA replacement.
#'
#' @param flow string, Transformation flow (Inp, Out)
#' @return  OPENPROM input data ITransfProcess.
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "ITransfProcess", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie

calcITransfProcess <- function(flow = "NULL") {
  transfProcessMapping <- toolGetMapping(
    name = "iea-transfProcess-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(flow, sep = ",") %>%
    filter(sector != "")

  fuelMap <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, variable = OPEN.PROM)

  transfProcess <- readSource("IEA2025", subset = unique(transfProcessMapping$flow)) %>%
    as.quitte() %>%
    filter(
      unit == "KTOE",
      product != "TOTAL",
      !is.na(value)
    ) %>%
    select(-variable) %>%
    mutate(unit = "Mtoe", value = value / 1000)

  # Conditional filtering based on flow argument
  transfProcess <- if (flow == "Inp") {
    filter(transfProcess, value < 0)
  } else if (flow == "Out") {
    filter(transfProcess, value > 0)
  } else {
    transfProcess
  }

  transfProcess <- transfProcess %>%
    inner_join(fuelMap, by = "product") %>%
    left_join(transfProcessMapping, by = "flow") %>%
    group_by(region, period, variable, sector) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    as.quitte() %>%
    as.magpie() %>%
    # FIXME: Proper impute must be done. For now fill with zero.
    toolCountryFill(fill = 0)

  transfProcess[is.na(transfProcess)] <- 0

  list(
    x = transfProcess,
    weight = NULL,
    unit = "Mtoe",
    description = "IEA; Transformation Processes"
  )
}
