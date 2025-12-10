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
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    helperDisaggregateOwnUse(fuelMap)

  ownUsePerSector <- ownUsePerSector %>%
    # Remove IEA sectors (e.g., EREFINER)
    select(-flow) %>%
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
helperDisaggregateOwnUse <- function(data, fuelMap) {
  # Define the transformation output mapping used for disaggregating sectors
  map <- list(
    "INDPROD" = "EOILGASEX"
    # "TREFINER" = "EREFINER",
    # "TCOALLIQ" = "ECOALLIQ"
  )

  flows <- unique(data[c("flow", "sector")])
  duplicated <- unique(flows$flow[duplicated(flows$flow)])
  duplicated <- setdiff(duplicated, unlist(map, use.names = FALSE))

  if (length(duplicated) > 0) {
    message(
      "Error in calcIDataOwnConsEne: The following flows have ",
      "no disaggregation mapping: ",
      paste(duplicated, collapse = ", ")
    )
    return(NULL)
  }

  EFTOEFAS <- toolGetMapping(
    name = "EFTOEFAS.csv",
    type = "blabla_export",
    where = "mrprom"
  )

  transformations <- readSource(
    "IEA2025",
    subset = names(map),
  ) %>%
    as.quitte() %>%
    filter(
      unit == "KTOE",
      product != "TOTAL",
      !is.na(value),
      value > 0
    ) %>%
    select(c("region", "period", "flow", "product", "value")) %>%
    mutate(
      value = value / 1000, # Convert to Mtoe
      flow = recode(flow, !!!map)
    ) %>%
    inner_join(fuelMap, by = "product") %>%
    left_join(EFTOEFAS, by = c("EFS" = "EF")) %>%
    group_by(region, period, flow, EFA) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  x <- data %>%
    left_join(
      transformations,
      by = c("region", "period", "flow", "sector" = "EFA")
    ) %>%
    group_by(region, period, flow, EFS) %>%
    # Create shares based on output transformation processes
    mutate(share = ifelse(!is.na(value.y), value.y / sum(value.y, na.rm = TRUE), 1)) %>%
    # Disaggregate own use sector of IEA to EF produced (e.g., EREFINER -> SLD,LQD)
    mutate(value = value.x * share) %>%
    ungroup() %>%
    select(region, period, flow, sector, EFS, value)
}
