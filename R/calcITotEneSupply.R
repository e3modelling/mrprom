#' calcITotEneSupply
#'
#' Compute total energy supply (primary or secondary) from IEA energy balances
#' and map results to OPEN-PROM fuel categories.
#' This function constructs the OPEN-PROM input variable ITotEneSupply,
#' which represents total energy supply in Mtoe. The type of energy supply
#' extracted depends on the subtype argument:
#' {"Primary"} → uses IEA flow {INDPROD}
#' {"TES"} → uses IEA total energy supply flow {TES}
#' The selected IEA data are filtered to exclude totals and missing values,
#' and restricted to observations reported in kilotonnes of oil equivalent
#' (Ktoe). Values are converted to Mtoe by dividing by 1000.
#' IEA fuel products are mapped to OPEN-PROM fuel categories using the
#' mapping file {prom-iea-fuelcons-mapping.csv}, which may contain
#' multiple IEA products per OPEN-PROM variable.
#' After mapping, data are aggregated by region, year, and fuel variable.
#' Missing values are filled with zero using {toolCountryFill()},
#' and remaining NA values are explicitly replaced by zero.
#'
#' @return  OPENPROM input data IPrimProd
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "ITotEneSupply", subtype = "Primary", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom tidyr separate_rows
#' @importFrom quitte as.quitte

calcITotEneSupply <- function(subtype) {
  if (subtype == "Primary") {
    flow <- "INDPROD"
  } else if (subtype == "TES") {
    flow <- "TES"
  } else {
    message("ERROR: Wrong subtype in ITotEneSupply")
    return(NULL)
  }

  fuelMap <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, variable = OPEN.PROM)

  data <- readSource("IEA2025", subset = flow) %>%
    as.quitte() %>%
    filter(unit == "KTOE", product != "TOTAL", !is.na(value)) %>%
    select(-variable) %>%
    mutate(unit = "Mtoe", value = value / 1000) %>%
    inner_join(fuelMap, by = "product") %>%
    group_by(region, period, variable) %>%
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
    description = "IEA; Primary Secondary Suppply"
  )
}
