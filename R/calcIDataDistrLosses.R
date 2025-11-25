#' calcIDataDistrLosses
#'
#' Use data to derive OPENPROM input parameter iDataDistrLosses
#'
#' @return  OPENPROM input data iDataDistrLosses
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataDistrLosses", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate inner_join
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie

calcIDataDistrLosses <- function() {
  fStartHorizon <- readEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fStartHorizon"]

  fuelMap <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, variable = OPEN.PROM)

  distrLosses <- readSource("IEA2025", subset = "DISTLOSS") %>%
    as.quitte() %>%
    filter(unit == "KTOE", product != "TOTAL", !is.na(value)) %>%
    select(-variable) %>%
    mutate(unit = "Mtoe", value = -value / 1000) %>%
    inner_join(fuelMap, by = "product") %>%
    group_by(region, period, variable) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    as.quitte() %>%
    as.magpie() %>%
    # FIXME: Proper impute must be done. For now fill with zero.
    suppressWarnings({
      toolCountryFill(fill = 0)
    })

  distrLosses[is.na(distrLosses)] <- 0
  list(
    x = distrLosses,
    weight = NULL,
    unit = "Mtoe",
    description = "IEA; Distribution Losses"
  )
}
