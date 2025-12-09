#' calcITransfProcess
#'
#' Use IEA transformation processes data to derive OPENPROM input parameter ITransfProcess.
#'
#' @param flow string, Transformation flow (Inp, Out)
#' @param subtype string, Sector (Total, CHP, DHP, etc. according to iea-transfProcess-mapping.csv)
#' @return  OPENPROM input data ITransfProcess.
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "ITransfProcess", subtype = "CHP", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie

calcITransfProcess <- function(subtype = "Total", flow = "Inp") {
  fStartHorizon <- readEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fStartHorizon"]

  transfProcessMapping <- toolGetMapping(
    name = "iea-transfProcess-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(Sector, sep = ",") %>%
    # Keep the sector based on the subtype if it is not "Total"
    filter(if (subtype != "Total") Name %in% subtype else TRUE)

  fuelMap <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, variable = OPEN.PROM)

  transfProcess <- readSource("IEA2025", subset = unique(transfProcessMapping$Sector)) %>%
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
    warning("Invalid flow argument. transfProcess is NULL.")
  }
  
  suppressMessages(
    suppressWarnings(
      transfProcess <- transfProcess %>%
        inner_join(fuelMap, by = "product") %>%
        group_by(region, period, variable) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
        as.quitte() %>%
        as.magpie() %>%
        # FIXME: Proper impute must be done. For now fill with zero.
        toolCountryFill(fill = 0)
    )
  )

  transfProcess[is.na(transfProcess)] <- 0

  list(
    x = transfProcess,
    weight = NULL,
    unit = "Mtoe",
    description = "IEA; Transformation Processes"
  )
}
