#' calcIFuelCons2
#'
#' Use ENERDATA, IEA, TREMOVE and NAVIGATE fuel consumption data to derive
#' OPENPROM input parameter iFuelConsXXX
#' The data for the years 2010 : 2021 is mainly from ENARDATA and IEA.
#' For the years 2021:2100 the data is mainly from NAVIGATE.
#' For TRANSE the data from 2021:2100 the data is mainly from TREMOVE.
#' (XXX: NENSE, INDSE, DOMSE, TRANSE).
#'
#' @param subtype string, OPENPROM sector (DOMSE, INDSE, NENSE, TRANSE)
#' @return  OPENPROM input data iFuelConsXXX
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IFuelCons2", subtype = "DOMSE", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_rows crossing
#' @importFrom magclass as.magpie
#' @importFrom eurostat get_eurostat

calcIFuelCons2 <- function(subtype = "ALL") {
  fStartHorizon <- readEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fStartHorizon"]

  sbsIEAtoPROM <- toolGetMapping(
    name = "prom-iea-sbs.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(c("IEA", "OPEN.PROM"), sep = ",") %>%
    rename(flow = IEA)

  fuelMap <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, variable = OPEN.PROM)

  # This block reads all IEA flows, filters the data,
  # and aggregates products and flows to OPEN-PROM EFs and demand subsectors
  # -----------------------------------------------------------------------------
  dataFuelCons <- readSource("IEA2025", subset = unique(sbsIEAtoPROM$flow)) %>%
    as.quitte() %>%
    filter(!is.na(value), value != 0, unit == "KTOE") %>%
    mutate(unit = "Mtoe", value = value / 1000) %>%
    select(region, period, product, flow, value)

  # Move Blast Furnace and coke ovens input fuels to Iron & Steel
  dataFuelCons <- helperSonja(fuelMap, dataFuelCons) %>%
    # map IEA products to OPEN-PROM EFs
    inner_join(fuelMap, by = "product") %>%
    # map IEA flows to OPEN-PROM subsectors
    inner_join(sbsIEAtoPROM, by = "flow", relationship = "many-to-many") %>%
    # Aggregate to OPEN-PROM's EFs & SBS
    group_by(region, period, OPEN.PROM, variable) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  fuelCons <- disaggregateTechs(dataFuelCons, fStartHorizon, fuelMap)
  if (subtype %in% c("INDSE", "DOMSE", "NENSE", "TRANSE")) {
    subtype <- toolGetMapping(paste0(subtype, ".csv"),
      type = "blabla_export",
      where = "mrprom"
    )[[1]]
    fuelCons <- filter(fuelCons, DSBS %in% subtype)
  }

  fuelCons <- fuelCons %>%
    as.quitte() %>%
    as.magpie()

  fuelCons[is.na(fuelCons)] <- 0
  suppressMessages(
    suppressWarnings(
      fuelCons <- toolCountryFill(fuelCons, fill = 0)
    )
  )

  list(
    x = fuelCons,
    weight = NULL,
    unit = "Mtoe",
    description = "IEA; Eurostat; Fuel Consumption"
  )
}

# Helpers --------------------------------------------------------------
helperSonja <- function(fuelMap, dataFuelCons) {
  transfProcess <- readSource("IEA2025", subset = c("TBLASTFUR", "TCOKEOVS")) %>%
    as.quitte() %>%
    filter(
      unit == "KTOE",
      product != "TOTAL",
      !is.na(value)
    ) %>%
    select(-variable) %>%
    mutate(unit = "Mtoe", value = value / 1000) %>%
    inner_join(fuelMap, by = "product")

  # Step 1
  shareCoalProducts <- transfProcess %>%
    filter(value < 0, variable %in% c("HCL", "LGN")) %>%
    group_by(region, period) %>%
    mutate(tot_value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(region, period, flow) %>%
    mutate(share = value / tot_value) %>%
    ungroup() %>%
    select(region, period, product, flow, share) %>%
    rename(transFlow = flow)

  test <- shareCoalProducts %>%
    pivot_wider(
      names_from  = period,
      values_from = share
    )
  write.csv(test, "step_1.csv", row.names = FALSE)

  # Step 2
  TotTransfBFGCOKE <- transfProcess %>%
    filter(value < 0, variable %in% c("HCL", "LGN")) %>%
    group_by(region, period) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  transfSector <- readSource("IEA2025", subset = c("MAINELEC", "MAINCHP", "AUTOCHP")) %>%
    as.quitte() %>%
    filter(
      unit == "KTOE",
      product != "TOTAL",
      !is.na(value)
    ) %>%
    select(-variable) %>%
    mutate(unit = "Mtoe", value = value / 1000) %>%
    # inner_join(fuelMap, by = "product") %>%
    filter(product %in% c("BLAST_FURNACE_GAS", "OTH_RECOVGASES", "COKE_OVEN_GAS"), value < 0) %>%
    left_join(TotTransfBFGCOKE, by = c("region", "period")) %>%
    mutate(eff = value.x / value.y) %>%
    select(region, period, product, flow, eff)

  test <- transfSector %>%
    pivot_wider(
      names_from  = period,
      values_from = eff
    )
  write.csv(test, "step_2.csv", row.names = FALSE)

  # Step 3
  TotalCoalCons <- transfSector %>%
    group_by(region, period, flow) %>%
    summarise(eff = sum(eff, na.rm = TRUE), .groups = "drop") %>%
    left_join(TotTransfBFGCOKE, by = c("region", "period")) %>%
    mutate(value = -eff * value) %>%
    select(-eff)

  coalCons <- TotalCoalCons %>%
    left_join(shareCoalProducts, by = c("region", "period"), relationship = "many-to-many") %>%
    mutate(value = value / share) %>%
    select(-share)

  test <- coalCons %>%
    mutate(value = value * 41.868) %>%
    pivot_wider(
      names_from  = period,
      values_from = value
    )
  write.csv(test, "step_3.csv", row.names = FALSE)

  # Step 4
  effDSBS <- dataFuelCons %>%
    filter(
      flow != "IRONSTL",
      product %in% c("BLAST_FURNACE_GAS", "OTH_RECOVGASES", "COKE_OVEN_GAS")
    ) %>%
    mutate(value = ifelse(value < 0, -value, value)) %>%
    left_join(TotTransfBFGCOKE, by = c("region", "period")) %>%
    mutate(eff = -value.x / value.y) %>%
    select(region, period, product, flow, eff)

  test <- effDSBS %>%
    pivot_wider(
      names_from  = period,
      values_from = eff
    )
  write.csv(test, "step_4.csv", row.names = FALSE)

  # Step 5
  coalConsDSBS <- effDSBS %>%
    group_by(region, period, flow) %>%
    summarise(eff = sum(eff, na.rm = TRUE), .groups = "drop") %>%
    left_join(TotTransfBFGCOKE, by = c("region", "period")) %>%
    mutate(value = -eff * value) %>%
    select(-eff) %>%
    left_join(shareCoalProducts, by = c("region", "period"), relationship = "many-to-many") %>%
    mutate(value = value / share) %>%
    select(-share)

  test <- coalConsDSBS %>%
    mutate(value = value * 41.868) %>%
    pivot_wider(
      names_from  = period,
      values_from = value
    )
  write.csv(test, "step_5.csv", row.names = FALSE)

  # Step 6
  temp <- coalCons %>%
    group_by(region, period, product, transFlow) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  newEneBalance <- coalConsDSBS %>%
    group_by(region, period, product, transFlow) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    full_join(temp, by = c("region", "period", "product", "transFlow")) %>%
    replace_na(list(value.x = 0, value.y = 0)) %>%
    mutate(value = value.x + value.y) %>%
    select(-c("value.x", "value.y"))

  test <- newEneBalance %>%
    mutate(value = value * 41.868) %>%
    pivot_wider(
      names_from  = period,
      values_from = value
    )
  write.csv(test, "step_6.csv", row.names = FALSE)

  # Step 7
  ownUseIS <- readSource("IEA2025", subset = c("EBLASTFUR", "ECOKEOVS")) %>%
    as.quitte() %>%
    filter(!is.na(value), value != 0, unit == "KTOE") %>%
    mutate(unit = "Mtoe", value = -value / 1000, flow = "IRONSTL") %>%
    select(region, period, product, flow, value)

  dataFuelCons <- dataFuelCons %>%
    left_join(ownUseIS, by = c("region", "period", "flow", "product")) %>%
    mutate(value = ifelse(is.na(value.y), value.x, value.x + value.y)) %>%
    select(c("region", "period", "product", "flow", "value"))
}

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

disaggregateTransportModes <- function(products, fStartHorizon) {
  # Get relative shares for each EF (products) for specific modes
  # using data from eurostat

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
