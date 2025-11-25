#' calcITotEneSupply
#'
#' Use IEA Total Energy Supply data to derive OPENPROM input parameters IPrimProd,ISecondaryProd.
#'
#' @return  OPENPROM input data IPrimProd, ISecondaryProd..
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "ITotEneSupply", subtype = "Primary", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom tidyr pivot_wider separate_rows
#' @importFrom quitte as.quitte

calcITotEneSupply <- function(subtype = "Primary") {
  primaryProducts <- c(
    "ANTHRACITE", "COKING_COAL", "OTH_BITCOAL", "HARDCOAL_ND",
    "SUB_BITCOAL", "BROWNCOAL_ND", "LIGNITE", "PEAT", "CRUDE_OIL",
    "NATURAL_GAS", "OIL_SHALE", "ETHANE", "NGL", "PRIMARY_SOLID_BIOFUEL",
    "SOLAR_PV", "SOLAR_THERMAL", "GEOTHERMAL", "TIDAL_WAVE_OCEAN", "WIND",
    "HYDRO", "NUCLEAR"
  )

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

  if (subtype == "Primary") {
    fuelMap <- filter(fuelMap, product %in% primaryProducts)
  } else if (subtype == "Secondary") {
    fuelMap <- filter(fuelMap, !(product %in% primaryProducts))
  }

  suppressWarnings({
    tes <- readSource("IEA2025", subset = "TES") %>%
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
  })
  
  tes[is.na(tes)] <- 0

  list(
    x = tes,
    weight = NULL,
    unit = "Mtoe",
    description = "IEA; Primary Secondary Suppply"
  )
}
