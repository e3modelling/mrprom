#' calcITotEneSupply
#'
#' Use IEA Total Energy Supply data to derive OPENPROM input parameter IPrimProd
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
  } else {
    flow <- "TES"
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
    filter(unit == "KTOE", product != "TOTAL", !is.na(value), value > 0) %>%
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
