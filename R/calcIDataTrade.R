#' calcIDataTrade
#'
#' Derive the OPENPROM input parameter iDataTrade from IEA energy balance trade data.
#' The dataset contains fuel import and export volumes by region and fuel type, expressed in Mtoe.
#' IEA fuel categories are mapped to OPENPROM fuel definitions and aggregated across trade flows.
#' Missing country values are currently filled with zero as a temporary imputation approach.
#'
#' @return magpie object with OPENPROM input data iDataTrade
#'
#' @author Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataTrade", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate inner_join if_else
#' @importFrom quitte as.quitte


calcIDataTrade <- function() {
  fuelMap <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, variable = OPEN.PROM)

  data <- readSource("IEA2025", subset = c("EXPORTS", "IMPORTS")) %>%
    as.quitte() %>%
    filter(
      unit == "KTOE",
      !(product %in% c("TOTAL", "RENEWABLES_TOTAL")),
      !is.na(value)
    ) %>%
    select(region, period, flow, product, value) %>%
    mutate(unit = "Mtoe", value = value / 1000) %>%
    inner_join(fuelMap, by = "product") %>%
    group_by(region, period, flow, variable) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    as.quitte() %>%
    as.magpie() %>%
    # FIXME: Proper impute must be done. For now fill with zero.
    toolCountryFill(fill = 0)

  data[is.na(data)] <- 0
  list(
    x = data,
    weight = NULL,
    unit = "Mtoe",
    description = "IEA; Fuel Exports"
  )
}
