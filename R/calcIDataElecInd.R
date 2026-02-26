#' calcIDataElecInd
#'
#' Use ENERDATA electricity production data to derive OPENPROM input parameter IDataElecInd
#'
#' @return  OPENPROM input data IDataElecInd
#'
#' @author Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataElecInd", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_rows
#'
calcIDataElecInd <- function() {
  CHPtoEF <- toolGetMapping(
    name = "CHPtoEF.csv",
    type = "blabla_export",
    where = "mrprom"
  ) %>%
    separate_rows(EF, sep = ",") %>%
    rename(variable = CHP)

  heat <- calcOutput(type = "IDataHeatProd", aggregate = FALSE) %>%
    as.quitte() %>%
    select(-variable) %>%
    rename(variable = tech)

  elec <- calcOutput(type = "IDataElecProd", mode = "CHP", aggregate = FALSE) %>%
    as.quitte() %>%
    select(-variable) %>%
    rename(EF = ef) %>%
    inner_join(CHPtoEF, by = "EF") %>%
    group_by(region, period, variable) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  data <- heat %>%
    inner_join(elec, by = c("region", "period", "variable")) %>%
    mutate(value = value.x / (value.y * 8.598 * 1e-5)) %>%
    select(region, period, variable, value) %>%
    # Impute based on global values
    left_join(helperGetWorldElecInd(elec, heat), by = c("period", "variable")) %>%
    mutate(value = ifelse(!is.finite(value.x) | value.x == 0, value.y, value.x)) %>%
    select(region, period, variable, value) %>%
    as.quitte() %>%
    as.magpie()

  weights <- as.quitte(elec) %>% as.magpie()
  list(
    x = data,
    weight = weights,
    unit = "ratio",
    description = "Steam / Elec ratio"
  )
}

# Helper --------------------------------------------------------
helperGetWorldElecInd <- function(elec, heat) {
  world <- heat %>%
    left_join(elec, by = c("region", "period", "variable")) %>%
    # mutate(value = ifelse(is.infinite(value), NA, 0)) %>%
    group_by(period, variable) %>%
    summarise(value = sum(value.x, na.rm = TRUE) / (sum(value.y, na.rm = TRUE) * 8.598 * 1e-5), .groups = "drop")
  return(world)
}
