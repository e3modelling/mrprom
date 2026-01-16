#' calcITransfProcess
#'
#' Use IEA transformation processes data to derive OPENPROM input parameter ITransfProcess.
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
