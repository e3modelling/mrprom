#' calcIDataFuelConsPerService
#'
#' @return  Magpie object
#'
#' @author Michael Madianos
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataFuelConsPerService", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter left_join mutate select %>% n_distinct
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIDataFuelConsPerService <- function() {
  INDDOM <- toolGetMapping("INDDOM.csv",
    type = "blabla_export",
    where = "mrprom",
  )$INDDOM

  INDDOMtoSERVS <- toolGetMapping("INDDOMtoSERVS.csv",
    type = "blabla_export",
    where = "mrprom",
  ) %>%
    separate_rows(IND_SERVS, sep = ",") %>%
    rename(dsbs = INDDOM, ind_servs = IND_SERVS)

  SFCGlobal <- calcOutput(type = "IDataIronSteelSFC", aggregate = FALSE) %>%
    as.quitte() %>%
    rename(dsbs = variable) %>%
    select(dsbs, ind_servs, inditech, ef, value) %>%
    rename(SFC = value)

  sharesEF <- calcOutput("iProductionIS", aggregate = FALSE) %>%
    as.quitte() %>%
    rename(prod = value) %>%
    select(region, period, dsbs, ind_servs, inditech, prod) %>%
    left_join(SFCGlobal, by = c("dsbs", "ind_servs", "inditech"), relationship = "many-to-many") %>%
    mutate(value = (prod + 1e-6) * SFC) %>%
    group_by(region, period, dsbs, ef) %>%
    mutate(value = value / sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(region, period, dsbs, ind_servs, inditech, ef, value)

  fuel <- calcOutput(type = "IFuelCons2", aggregate = FALSE) %>%
    as.quitte() %>%
    select(region, period, dsbs, ef, value) %>%
    filter(dsbs %in% INDDOM) %>%
    inner_join(
      sharesEF,
      by = c("region", "period", "dsbs", "ef"),
      relationship = "many-to-many"
    ) %>%
    mutate(value = value.x * value.y) %>%
    select(-c("value.x", "value.y")) %>%
    as.quitte() %>%
    as.magpie()

  list(
    x = fuel,
    weight = NULL,
    unit = "Mtoe",
    description = "Fuel consumption per service"
  )
}
