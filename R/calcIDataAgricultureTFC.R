#' calcIDataAgricultureTFC
#'
#' Use IFuelCons and data from AGENRES to dissagregate the consumption
#' in various agriculture modes.
#'
#' @return  OPENPROM historical shares of agriculture.
#'
#' @author Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataAgricultureTFC", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_rows crossing
#' @importFrom magclass as.magpie

calcIDataAgricultureTFC <- function() {
  fuelMap <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, ef = OPEN.PROM)

  fuelCons <- readSource("IEA2025", subset = c("AGRI_FOREST", "FISHING")) %>%
    as.quitte() %>%
    filter(unit == "KTOE") %>%
    mutate(
      unit = "Mtoe",
      value = value / 1000
    ) %>%
    select(-variable) %>%
    # map IEA products to OPEN-PROM EFs
    inner_join(fuelMap, by = "product") %>%
    group_by(region, period, unit, flow, ef) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    rename(variable = flow)

  # --------- Disaggregate Forestry, Agriculture from AGRI_FOREST --------
  SFCGlobal <- calcOutput(type = "IDataAgricultureEff", aggregate = FALSE) %>%
    as.quitte() %>%
    select(region, period, variable, agritech, ef, value) %>%
    rename(SFC = value)

  sharesEF <- calcOutput("IDataAgricultureService", aggregate = FALSE) %>%
    as.quitte() %>%
    rename(prod = value) %>%
    select(region, period, variable, prod) %>%
    left_join(SFCGlobal, by = c("region", "period", "variable"), relationship = "many-to-many") %>%
    mutate(value = (prod + 1e-6) * SFC) %>%
    filter(!(variable == "FISHING")) %>%
    group_by(region, period, ef) %>%
    mutate(value = value / sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(region, period, variable, agritech, ef, value)

  agriculture <- fuelCons %>%
    filter(variable == "AGRI_FOREST") %>%
    select(-variable) %>%
    inner_join(sharesEF, by = c("region", "period", "ef"), relationship = "many-to-many") %>%
    mutate(value = value.x * value.y) %>%
    select(region, period, variable, agritech, ef, value)

  AGRITECHTOEF <- toolGetMapping("AGRITECHTOEF.csv",
    type = "blabla_export",
    where = "mrprom",
  ) %>%
    separate_rows(EF, sep = ",") %>%
    rename(
      ef = EF,
      agritech = AGRITECH
    )

  fishing <- fuelCons %>%
    select(-unit) %>%
    filter(variable == "FISHING") %>%
    inner_join(AGRITECHTOEF, by = c("ef"))

  magpie_object <- rbind(agriculture, fishing) %>%
    as.quitte() %>%
    as.magpie()

  magpie_object[is.na(magpie_object)] <- 0

  list(
    x = magpie_object,
    weight = NULL,
    unit = "Mtoe",
    description = "IEA; AGENRES; Disaggregate Agriculture"
  )
}
