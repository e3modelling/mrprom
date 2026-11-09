#' calcIFuelConsNEN
#' 
#' @return  OPENPROM input data iFuelConsNEN
#'
#' @author Michael Madianos, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IFuelConsNEN", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_rows crossing
#' @importFrom magclass as.magpie
#' @importFrom eurostat get_eurostat

calcIFuelConsNEN <- function() {
  fStartHorizon <- toolReadEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fStartHorizon"]
  fEndY <- toolReadEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fEndY"]
  
  sbsIEAtoPROMNEN <- toolGetMapping(
    name = "prom-iea-sbs-NEN.csv",
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
  
  # ---------------------------------------------------------------------
  dataFuelCons <- readSource("IEA2025", subset = unique(sbsIEAtoPROMNEN$flow)) %>%
    as.quitte() %>%
    filter(value != 0, unit == "KTOE") %>%
    mutate(
      unit = "Mtoe",
      value = value / 1000,
      # value = ifelse(value < 0, -value, value)
    ) %>%
    select(-variable) %>%
    # map IEA products to OPEN-PROM EFs
    inner_join(fuelMap, by = "product") %>%
    # map IEA flows to OPEN-PROM subsectors
    inner_join(sbsIEAtoPROMNEN, by = "flow", relationship = "many-to-many")
  
  dataFuelCons <- RESTNEN(dataFuelCons, fuelMap) %>%
    # Aggregate to OPEN-PROM's EFs & SBS
    group_by(region, period, OPEN.PROM, variable) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    rename(DSBS = OPEN.PROM, EF = variable)
  #------------------------------------------------------------------------
  fuelCons <- dataFuelCons %>%
    as.quitte() %>%
    as.magpie()
  
  fuelCons[is.na(fuelCons)] <- 0
  fuelCons <- toolCountryFill(fuelCons, fill = 0)
  
  list(
    x = fuelCons,
    weight = NULL,
    unit = "Mtoe",
    description = "IEA; Fuel Consumption"
  )
}

# Helpers --------------------------------------------------------------
RESTNEN <- function(dataFuelCons, fuelMap) {

  NENPCH <- dataFuelCons %>%
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
    mutate(OPEN.PROM = "OI") %>%
    select(region, period, OPEN.PROM, product, value) %>%
    # map IEA products to OPEN-PROM EFs
    inner_join(fuelMap, by = "product") %>%
    as.quitte() %>%
    mutate(flow = "REST_NEN") %>%
    rename(OPEN.PROM = open.prom)
  
  # rbind rest NEN with dataFuelCons without NE_TOT
  AddNEN <- rbind(NENPCH, dataFuelCons[!dataFuelCons[["flow"]] %in% c("NE_TOT"), ])
  
  return(AddNEN)
}
