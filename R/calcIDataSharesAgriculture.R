#' calcIDataSharesAgriculture
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
#' a <- calcOutput(type = "IDataSharesAgriculture", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_rows crossing
#' @importFrom magclass as.magpie
#' @importFrom eurostat get_eurostat

calcIDataSharesAgriculture <- function() {
  fEndY <- toolReadEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fEndY"]

  data <- readSource("AGENRES") %>%
    collapseDim(dim = 3.2)
  getYears(data) <- fEndY

  fuelMap <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, ef = OPEN.PROM)
  
  fuelCons <- readSource("IEA2025", subset = c("AGRI_FOREST", "FISHING")) %>%
    as.quitte() %>%
    filter(value != 0, unit == "KTOE") %>%
    mutate(unit = "Mtoe", value = value / 1000) %>%
    select(-variable) %>%
    # map IEA products to OPEN-PROM EFs
    inner_join(fuelMap, by = "product") %>%
    group_by(region, period, unit, flow, ef) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  # ------------------------------------------------------------------
  mapping <- data.frame(
    Animal = c("Dairy Cows", "Ruminants", "Pigs", "Broilers", "Laying hens", "Crops", "Greenhouses"),
    Category = c(rep("Livestock", 5), "Crops", "Climate")
  )

  x <- toolAggregate(data, rel = mapping, from = "Animal", to = "Category", dim = 3.1, partrel = FALSE)
  getNames(x)[getNames(x) == "Crops.Irrigation"] <- "Irrigation.Irrigation"
  getNames(x)[getNames(x) == "Crops.Drying"] <- "PostHarvesting.Drying"
  getNames(x)[getNames(x) == "Crops.Feed preparation"] <- "PostHarvesting.Feed preparation"
  getNames(x)[getNames(x) == "Livestock.Cooling"] <- "Climate.Cooling"
  getNames(x)[getNames(x) == "Livestock.Heating / wentilation"] <- "Climate.Heating / wentilation"
  getNames(x)[getNames(x) == "Livestock.Water pumping"] <- "Irrigation.Water pumping"
  getNames(x)[getNames(x) == "Livestock.Watering"] <- "Irrigation.Watering"
  getNames(x)[getNames(x) == "Livestock.Water heating"] <- "Irrigation.Water heating"
  getNames(x)[getNames(x) == "Livestock.Other / Heating"] <- "Climate.Other / Heating"


  mappingFuel <- data.frame(
    Type = c(
      "Milking", "Cooling", "Air compress", "Feeding", "Water pumping", "Watering",
      "Manure removal", "Other", "Other / Heating", "Water heating", "Heating / wentilation",
      "Tillage", "Irrigation", "Crop maintenance", "Harvesting", "Drying", "Feed preparation",
      "On farm transportation", "Rest", "Heating"
    ),
    Fuel = c(rep("ELC", 9), "Thermal", "Thermal", rep("GDO", 7), "ELC", "Thermal")
  )
  y <- toolAggregate(x, rel = mappingFuel, from = "Type", to = "Fuel", dim = 3.2, partrel = FALSE)


  z <- as.quitte(y) %>%
    rename(ef = type) %>%
    group_by(region, period, ef) %>%
    mutate(share = value / sum(value)) %>%
    ungroup() %>%
    select(region, variable, ef, share)

  final <- z %>%
    bind_rows(
      z %>%
        filter(ef == "GDO") %>%
        mutate(ef = "BGDO")
    ) %>%
    bind_rows(
      z %>%
        filter(ef == "Thermal") %>%
        rename(type_old = ef) %>%
        select(-type_old) %>%
        tidyr::crossing(ef = c("LGN", "HCL", "NGS", "BMSWAS", "LPG", "BGAS", "STE", "GEO", "SOL", "RFO", "OGS"))
    ) %>%
    filter(ef != "Thermal") %>%
    mutate(flow = "AGRI_FOREST")

  fuelCons2 <- fuelCons %>%
    left_join(final, by = c("region", "ef", "flow"), relationship = "many-to-many") %>%
    mutate(value = ifelse(flow == "AGRI_FOREST", value * share, value)) %>%
    select(region, period, flow, variable, ef, value)


  a <- fuelCons %>%
    filter(!is.na(value)) %>%
    group_by(period, variable, ef) %>%
    summarise(value = sum(value, na.rm = T), .groups = "drop")

  p <- a %>%
    filter(period == 2023) %>%
    ggplot(aes(x = variable, y = value, fill = ef)) +
    geom_bar(stat = "identity", width = 0.7) +
    labs(
      x = "Variable",
      y = "Consumption [Mtoe]",
      fill = "Energy form",
      title = "EU27 Agriculture, Forestry, and Fishing fuel consumption (2023)"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1),
      legend.position = "right"
    ) +
    scale_fill_brewer(palette = "Set3")

  ggsave("my_plot.png", plot = p, width = 8, height = 6, dpi = 300)

  final <- new.magpie(
    cells_and_regions = getRegions(data),
    years = fEndY,
    names = getItems(data, 3),
    fill = NA
  )

  list(
    x = ,
    weight = NULL,
    unit = "Mtoe",
    description = "IEA; AGENRES; Disaggregate Agriculture"
  )
}

# Helpers --------------------------------------------------------------
disaggregateTechs <- function(dataFuelCons, fStartHorizon, fuelMap) {
  # Disaggregate Fuels for Transport sector (e.g., GSL ROAD -> PC,PB,GU)
  products <- unique(dataFuelCons$variable)
  sharesTransp <- disaggregateTransportModes(products, fStartHorizon)
  dataFuelConsIEA <- dataFuelCons %>%
    left_join(
      sharesTransp,
      by = c("period", "OPEN.PROM", "variable"),
      relationship = "many-to-many"
    ) %>%
    mutate(value = ifelse(is.na(value.y), value.x, value.x * value.y)) %>%
    select(-c("value.x", "value.y")) %>%
    rename(DSBS = OPEN.PROM, EF = variable)
}

processNetotNenpch <- function(dataFuelCons, fuelMap) {
  # filter IEA with "NEN", "PCH"
  NENPCH <- dataFuelCons[dataFuelCons[["OPEN.PROM"]] %in% c("NEN", "PCH"), ] %>%
    # classify flows
    mutate(flow = case_when(
      flow == "NE_TOT" ~ "NE_TOT",
      TRUE ~ "PCHNEN"
    )) %>%
    # summarise values
    group_by(region, period, flow, product) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    # reshape
    pivot_wider(
      names_from = flow,
      values_from = value
    ) %>%
    # fill NA and compute difference
    mutate(
      NE_TOT = replace_na(NE_TOT, 0),
      PCHNEN = replace_na(PCHNEN, 0),
      value  = NE_TOT - PCHNEN
    ) %>%
    # output format
    mutate(OPEN.PROM = "NEN") %>%
    select(region, period, OPEN.PROM, product, value) %>%
    # map IEA products to OPEN-PROM EFs
    inner_join(fuelMap, by = "product") %>%
    as.quitte() %>%
    mutate(unit = "Mtoe") %>%
    mutate(flow = "REST_NEN") %>%
    rename(OPEN.PROM = open.prom)

  # rbind rest NEN with dataFuelCons without NE_TOT
  AddNEN <- rbind(NENPCH, dataFuelCons[!dataFuelCons[["flow"]] %in% c("NE_TOT"), ])

  return(AddNEN)
}

disaggregateTransportModes <- function(products, fStartHorizon) {
  # Get relative shares for each EF (products) for specific modes

  mapEuroToOPEN <- toolGetMapping(
    name = "prom-eurostat-fuelcons-transport.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(c("EUROSTAT", "OPEN.PROM"), sep = ",") %>%
    rename(product = EUROSTAT)

  mapRegions <- toolGetMapping(
    name = "regionmappingOPDEV3.csv",
    type = "regional",
    where = "mrprom"
  ) %>%
    select(-"Region.Code") %>%
    rename(
      geo = Full.Country.Name,
      region = ISO3.Code
    )

  dataConsEuro <- get_eurostat(
    "nrg_d_traq",
    type = "label",
    time_format = "raw",
    select_time = "Y",
    stringsAsFactors = TRUE
  ) %>%
    filter(
      unit == "Terajoule",
      TIME_PERIOD >= fStartHorizon
    ) %>%
    # Transform into proper naming conventions
    mutate(
      tra_mode = recode(tra_mode, "Passenger" = "P", "Freight" = "G"),
      nrg_bal = recode(nrg_bal,
        "Final consumption - transport sector - domestic aviation - energy use" = "A",
        "Final consumption - transport sector - domestic navigation - maritime - energy use" = "N",
        "Final consumption - transport sector - road - cars and vans - energy use" = "C",
        "Final consumption - transport sector - rail - conventional - energy use" = "T",
        "Final consumption - transport sector - road - public - energy use" = "B"
      )
    ) %>%
    # Keep only relevant rows
    filter(
      nrg_bal %in% c("A", "N", "C", "T", "B"),
      !(tra_mode == "Total" & nrg_bal != "B") # Busses are under the "Total" tra_mode
    ) %>%
    # Renamings
    mutate(
      mode = paste0(tra_mode, nrg_bal),
      mode = ifelse(mode == "GC", "GU", mode),
      mode = ifelse(mode == "TotalB", "PB", mode)
    ) %>%
    rename(product = siec)

  dataConsEuro <- dataConsEuro %>%
    # Transform region names & product names (e.g., Electricity -> ELC)
    inner_join(mapRegions, by = "geo") %>%
    inner_join(mapEuroToOPEN, by = "product") %>%
    select(region, TIME_PERIOD, mode, OPEN.PROM, values) %>%
    # Aggregate for all regions
    group_by(TIME_PERIOD, mode, OPEN.PROM) %>%
    summarise(values = sum(values, na.rm = TRUE), .groups = "drop") %>%
    rename(
      product = OPEN.PROM,
      flow = mode,
      period = TIME_PERIOD
    )

  modesToVariables <- c(
    "GN" = "DOMESNAV", "PN" = "DOMESNAV",
    "PT" = "RAIL", "GT" = "RAIL",
    "PC" = "ROAD", "PB" = "ROAD", "GU" = "ROAD",
    "PA" = "DOMESAIR"
  )
  shares <- dataConsEuro %>%
    mutate(
      variable = recode(flow, !!!modesToVariables),
      period = as.numeric(as.character(period))
    ) %>%
    filter(
      product %in% products,
      variable %in% unique(unname(modesToVariables))
    ) %>%
    group_by(period, product, variable) %>%
    mutate(
      # If no consumption, disaggregate uniformly
      values = ifelse(values == 0, 1e-6, values),
      value = values / sum(values, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(-c("values", "variable")) %>%
    interpolate_missing_periods(
      period = seq(fStartHorizon, 2025, 1),
      expand.values = TRUE
    ) %>%
    rename(
      OPEN.PROM = flow,
      variable = product
    )
}
