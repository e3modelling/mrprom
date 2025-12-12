#' calcIDataOwnConsEne
#'
#' Use data from IEA to derive OPENPROM input parameter IDataOwnConsEne
#' This dataset includes own consumption values for each region and energy sector in Mtoe.
#'
#' @return magpie object with OPENPROM input data IDataOwnConsEne
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataOwnConsEne", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate inner_join n left_join
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom tidyr separate_rows

calcIDataOwnConsEne <- function() {
  fuelMap <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, EFS = "OPEN.PROM")

  supplySecs <- toolGetMapping(
    name = "prom-iea-supply-sectors-own-use.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    filter(sector != "") %>%
    separate_rows("sector", sep = ",")

  # This block reads all IEA own consumption flows, maps:
  # - products to OPEN-PROM EFs
  # - Own use flows to OPEN-PROM supply sectors
  # and dissagregates own use in case of one flow-to-many sectors
  # (EPOWERPLT -> PG,CHP,STEAMP) based on transformation outputs
  ownUsePerSector <- readSource("IEA2025", subset = unique(supplySecs$flow)) %>%
    as.quitte() %>%
    filter(
      unit == "KTOE",
      !product %in% c("TOTAL", "RENEWABLES_TOTAL"),
      !is.na(value),
      value < 0
    ) %>%
    select(region, period, unit, flow, product, value) %>%
    mutate(unit = "Mtoe", value = -value / 1000) %>%
    inner_join(fuelMap, by = "product") %>%
    left_join(supplySecs, by = "flow", relationship = "many-to-many") %>%
    group_by(region, period, flow, sector, EFS) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  shares <- helperDisaggregateOwnUse(fuelMap)

  ownUsePerSector <- ownUsePerSector %>%
    left_join(shares, by = c("region", "period", "flow", "sector")) %>%
    mutate(
      share = ifelse(is.na(share), 0, share),
      value = value * share
    ) %>%
    # Aggregate per final sector (LQD, SLD, GAS, PG, H2P, STE)
    group_by(region, period, sector, EFS) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    as.quitte() %>%
    as.magpie() %>%
    # FIXME: Proper impute must be done. For now fill with zero.
    toolCountryFill(fill = 0)

  ownUsePerSector[is.na(ownUsePerSector)] <- 0

  list(
    x = ownUsePerSector,
    weight = NULL,
    unit = "Mtoe",
    description = "IEA; Own consumption per sector"
  )
}

# Helpers ------------------------------------------------------
helperDisaggregateOwnUse <- function(fuelMap) {
  # Define the transformation output mapping used for disaggregating sectors
  map <- data.frame(
    flow = c("INDPROD", "MAINELEC", "MAINCHP", "MAINHEAT"),
    ownUse = c("EOILGASEX", "EPOWERPLT", "EPOWERPLT", "EPOWERPLT"),
    sector = c("temp", "PG", "CHP", "STEAMP")
  )

  EFTOEFAS <- toolGetMapping(
    name = "EFTOEFAS.csv",
    type = "blabla_export",
    where = "mrprom"
  )

  transformations <- readSource(
    "IEA2025",
    subset = map$flow,
  ) %>%
    as.quitte() %>%
    filter(
      unit == "KTOE",
      product != "TOTAL",
      !is.na(value),
      value > 0
    ) %>%
    select(c("region", "period", "flow", "product", "value")) %>%
    left_join(map, by = "flow") %>%
    inner_join(fuelMap, by = "product") %>%
    left_join(EFTOEFAS, by = c("EFS" = "EF")) %>%
    filter(!(ownUse == "EOILGASEX" & !(EFA %in% c("LQD", "SLD")))) %>%
    mutate(
      value = value / 1000, # Convert to Mtoe
      flow = ownUse,
      EFA = ifelse(sector == "temp", EFA, sector)
    ) %>%
    group_by(region, period, flow, EFA) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  # Create shares based on output transformation processes
  shares <- transformations %>%
    group_by(region, period, flow) %>%
    mutate(share = value / sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    select(region, period, flow, EFA, share) %>%
    rename(sector = EFA)
}
