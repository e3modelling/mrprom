#' calcIPrimProd
#'
#' Use ENERDATA Primary production data to derive OPENPROM input parameter IPrimProd.
#'
#' @return  OPENPROM input data IPrimProd.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IPrimProd", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom tidyr pivot_wider separate_rows
#' @importFrom quitte as.quitte


calcIPrimProd <- function() {
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

  primaryProd <- readSource("IEA2025", subset = "INDPROD") %>%
    as.quitte() %>%
    filter(unit == "KTOE", product != "TOTAL", !is.na(value)) %>%
    select(-variable) %>%
    mutate(unit = "Mtoe", value = value / 1000) %>%
    inner_join(fuelMap, by = "product") %>%
    group_by(region, period, variable) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    as.quitte() %>%
    as.magpie() %>%
    # FIXME: Proper impute must be done. For now fill with zero.
    toolCountryFill(fill = 0)

  primaryProd[is.na(primaryProd)] <- 0

  list(
    x = primaryProd,
    weight = NULL,
    unit = "Mtoe",
    description = "IEA; Primary production"
  )
}
