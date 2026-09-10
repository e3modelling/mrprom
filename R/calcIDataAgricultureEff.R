#' calcIDataAgricultureEff
#'
#' @return  Magpie object with the FAOProductionCrops
#'
#' @author Fotis Sioutas
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataAgricultureEff", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter left_join mutate select %>%
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom R.utils isZero

calcIDataAgricultureEff <- function() {
  fEndY <- toolReadEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fEndY"]

  TFC <- calcOutput(type = "IDataAgricultureTFC", aggregate = FALSE) %>%
    as.quitte() %>%
    select(region, period, variable, ef, value)

  service <- calcOutput(type = "IDataAgricultureService", aggregate = FALSE) %>%
    as.quitte() %>%
    select(region, period, variable, value)

  dataGlobal <- service %>%
    left_join(TFC, by = c("region", "period", "variable")) %>%
    group_by(period, variable, ef) %>%
    summarise(
      service = sum(value.x, na.rm = TRUE),
      fuel = sum(value.y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(global = fuel / service) %>%
    select(period, variable, ef, global)

  data <- service %>%
    left_join(TFC, by = c("region", "period", "variable")) %>%
    left_join(dataGlobal, by = c("period", "ef", "variable")) %>%
    filter(period <= fEndY) %>%
    mutate(
      value = value.y / value.x,
      value = ifelse(is.nan(value), global, value)
    ) %>%
    select(region, period, variable, ef, value) %>%
    as.quitte() %>%
    as.magpie()

  weights <- as.quitte(service) %>%
    filter(period <= fEndY) %>%
    as.magpie()

  list(
    x = data,
    weight = weights,
    unit = "Mtoe / [Activity]",
    description = "FAOProductionCrops"
  )
}
