#' calcIDataHeatProd
#'
#' Use IEA heat production data to derive OPENPROM input parameter iDataHeatProd
#'
#' @return  OPENPROM input data iDataHeatProd
#'
#' @author Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataHeatProd", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select summarise left_join full_join
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_rows

calcIDataHeatProd <- function() {
  CHPtoEF <- toolGetMapping(
    name = "CHPtoEF.csv",
    type = "blabla_export",
    where = "mrprom"
  ) %>%
    separate_rows(EF, sep = ",")

  DHtoEF <- toolGetMapping(
    name = "DHtoEF.csv",
    type = "blabla_export",
    where = "mrprom"
  ) %>%
    separate_rows(EF, sep = ",")

  map <- left_join(DHtoEF, CHPtoEF, by = "EF")

  sectors <- list(
    CHP = c("HEMAINC", "HEAUTOC"),
    STEAMP = c("HEMAINH", "HEAUTOH")
  ) %>%
    stack() %>%
    rename(sector = ind, flow = values)

  fuelMap <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, EF = OPEN.PROM)

  data <- readSource("IEA2025", subset = unique(sectors$flow)) %>%
    as.quitte() %>%
    filter(unit == "KTOE") %>%
    mutate(
      value = value / 1000,
      unit = "Mtoe"
    ) %>%
    select(-variable) %>%
    # map IEA products to OPEN-PROM EFs
    inner_join(fuelMap, by = "product") %>%
    inner_join(sectors, by = "flow") %>%
    inner_join(map, by = "EF") %>%
    mutate(
      tech = case_when(
        sector == "CHP" ~ as.character(CHP),
        sector == "STEAMP" ~ as.character(DH),
        TRUE ~ NA
      )
    ) %>%
    # Aggregate to OPEN-PROM's EFs & SBS
    group_by(region, period, tech) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    as.quitte() %>%
    as.magpie()

  list(
    x = data,
    weight = NULL,
    unit = "Mtoe",
    description = "IEA; Heat production"
  )
}
