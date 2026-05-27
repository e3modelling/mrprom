#' calcTFuelCons
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
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "TFuelCons", subtype = "DOMSE", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select last
#' @importFrom tidyr pivot_wider separate_rows crossing
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom eurostat get_eurostat


calcTFuelCons <- function(subtype = "TRANSE") {
  
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
    inner_join(sbsIEAtoPROM, by = "flow", relationship = "many-to-many") %>%
    # Aggregate to OPEN-PROM's EFs & SBS
    group_by(region, period, OPEN.PROM, variable) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  
  # use it for Navigate
  i <- subtype
  
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
  fuelCons <- toolCountryFill(fuelCons, fill = 0)
  x <- fuelCons
  
  # set NA to 0
  x[is.na(x)] <- 10^-6
  
  Navigate <- calcOutput(type = "Navigate", subtype = i, aggregate = FALSE)
  Navigate <- as.quitte(Navigate)
  
  Primes <- calcOutput(type = "Primes", aggregate = FALSE)
  Primes <- as.quitte(Primes[,,intersect(getItems(Primes,3.1),subtype)])
  
  #join Primes and Navigate
  Primes_Nav <- full_join(Primes, Navigate, by = c("model", "scenario", "region", "period", "variable", "unit", "new")) %>%
    mutate(value = ifelse(value.x == 10^-6 | is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  Primes_Nav <- as.magpie(Primes_Nav)
  Primes_Nav[is.na(Primes_Nav)] <- 10^-6
  Primes_Nav <- as.quitte(Primes_Nav)
  z <- Primes_Nav
  names(z) <- sub("variable", "dsbs", names(z))
  names(z) <- sub("new", "ef", names(z))
  z <-  select(z, -c("unit"))
  
  #join IEA and Primes_Nav
  qx <- full_join(as.quitte(x), as.quitte(z), by = c("model", "scenario", "region", "period", "dsbs", "unit", "ef")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  
  x <- as.quitte(qx) %>% as.magpie()
  # set NA to 0
  x[is.na(x)] <- 10^-6
  x <- x[,fStartHorizon : gsub("y","",last(getYears(x))),]
  
  #extrapolate_IEA_if_there_are_not_data_from_Navigate
  
  extrapolate <- x[,fStartHorizon : 2023,]
  
  extrapolate_x <- as.quitte(extrapolate) %>%
    interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
  
  qextrapolate_x_original <- full_join(as.quitte(x), extrapolate_x, by = c("model", "scenario", "region", "period", "variable", "unit", "dsbs", "ef")) %>%
    mutate(value = ifelse(value.x == 10^-6, value.y, value.x)) %>%
    select(-c("value.x", "value.y", "unit"))
  
  hist_all <- as.quitte(qextrapolate_x_original) %>%
    filter(period <= 2023) %>%
    mutate(projected_value = value) 
  
  hist_2023 <- as.quitte(qextrapolate_x_original) %>%   # original full dataset
    filter(period == 2023) %>%
    select(region, dsbs, ef, value_2023 = value)
  
  qextrapolate_x <- as.quitte(qextrapolate_x_original) %>%
    filter(period > 2023) %>%
    arrange(region, dsbs, ef, period) %>%    
    group_by(region, dsbs, ef) %>%           
    mutate(
      prev_value = lag(value),
      diff_ratio = (value - prev_value) / if_else(prev_value == 0, 1, prev_value),
      diff_ratio = if_else(is.na(diff_ratio), 0, diff_ratio)
    ) %>%
    ungroup()
  
  qextrapolate_future <- qextrapolate_x %>%
    left_join(hist_2023, by = c("region", "dsbs", "ef"))
  
  qextrapolate_future <- qextrapolate_future %>%
    mutate(factor = 1 + diff_ratio)
  
  qextrapolate_future <- qextrapolate_future %>%
    group_by(region, dsbs, ef) %>%
    arrange(period) %>%
    mutate(
      projected_value = value_2023 * cumprod(factor)
    ) %>%
    ungroup()
  
  final <- bind_rows(
    hist_all,
    qextrapolate_future   # contains projected values >2023
  ) %>%
    arrange(region, dsbs, ef, period)
  
  final <- final %>%
    mutate(
      value = projected_value    # rename to a unified single column
    ) %>%
    select(region, dsbs, ef, period, value)
  
  x <- as.quitte(final) %>% as.magpie()
  
  # set NA to 0
  x[is.na(x)] <- 10^-6
  x[x==0] <- 10^-6
  
  if (i == "TRANSE") {
    x["MLT",,"PT"] <- 10^-6
    x["CYP",,"PT"] <- 10^-6
  }
  
  list(x = x,
       weight = NULL,
       unit = "various",
       description = "ENERDATA, IEA, TREMOVE and NAVIGATE; fuel consumption in XXX sector")
  
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

