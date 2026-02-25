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
  
  dataFuelCons <- readSource("IEA2025", subset = unique(sbsIEAtoPROM$flow)) %>%
    as.quitte() %>%
    filter(value != 0, unit == "KTOE") %>%
    mutate(unit = "Mtoe", value = value / 1000) %>%
    select(-variable) %>%
    # map IEA products to OPEN-PROM EFs
    inner_join(fuelMap, by = "product") %>%
    # map IEA flows to OPEN-PROM subsectors
    inner_join(sbsIEAtoPROM, by = "flow", relationship = "many-to-many")
    dataFuelCons <- processNetotNenpch(dataFuelCons) %>%
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

processNetotNenpch <- function(dataFuelCons) {
  # filter IEA with "NEN", "PCH"
  NENPCH <- dataFuelCons[dataFuelCons[["OPEN.PROM"]] %in% c("NEN", "PCH"),] %>%
    
    # classify flows
    mutate(flow = case_when(
      flow == "NE_TOT" ~ "NE_TOT",
      TRUE                  ~ "PCHNEN"
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
    select(region, period, OPEN.PROM, product, value)  %>%
    # map IEA products to OPEN-PROM EFs
    inner_join(fuelMap, by = "product") %>% as.quitte()  %>%
    mutate(unit = "Mtoe")  %>%
    mutate(flow = "REST_NEN")  %>%
    rename(OPEN.PROM = open.prom)
  
  # rbind rest NEN with dataFuelCons without NE_TOT
  AddNEN <- rbind(NENPCH, dataFuelCons[! dataFuelCons[["flow"]] %in% c("NE_TOT"),])
  
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
