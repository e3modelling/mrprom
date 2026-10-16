#' calcIDataIronSteelSFC
#'
#' @return  Magpie object
#'
#' @author Michael Madianos
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataIronSteelSFC", aggregate = FALSE)
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

  sharesEF <- calcOutput(type = "IDataIronSteelSFC", aggregate = FALSE) %>%
    as.quitte() %>%
    select(variable, ind_servs, inditech, ef, value) %>%
    group_by(variable, ef) %>%
    mutate(value = value / sum(value, na.rm = T)) %>%
    ungroup() %>%
    rename(dsbs = variable)

  fuel <- calcOutput(type = "IFuelCons2", aggregate = TRUE) %>%
    as.quitte() %>%
    select(region, period, dsbs, ef, value) %>%
    filter(dsbs %in% INDDOM) %>%
    left_join(INDDOMtoSERVS, by = c("dsbs"), relationship = "many-to-many") %>%
    left_join(sharesEF, by = c("dsbs", "ind_servs", "ef"), relationship = "many-to-many") %>%
    filter(dsbs == "IS")

  list(
    x = fuel,
    weight = NULL,
    unit = "Mtoe",
    description = "Fuel consumption per service"
  )
}
