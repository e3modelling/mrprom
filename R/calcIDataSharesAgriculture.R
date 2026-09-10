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
    filter(unit == "KTOE") %>%
    mutate(
      unit = "Mtoe",
      value = value / 1000
    ) %>%
    select(-variable) %>%
    # map IEA products to OPEN-PROM EFs
    inner_join(fuelMap, by = "product") %>%
    group_by(region, period, unit, flow, ef) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  # --------- Disaggregate Forestry, Agriculture from AGRI_FOREST --------
  ForestryConsEstimation <- calcOutput(type = "IDataAgricultureService", aggregate = FALSE) %>%
    as.quitte() %>%
    filter(variable == "FORESTRY") %>%
    mutate(
      forestryCons = value * 1.72 * 1e-6
    ) %>%
    select(region, period, forestryCons)

  shareForestry <- fuelCons %>%
    filter(flow == "AGRI_FOREST") %>%
    left_join(ForestryConsEstimation, by = c("region", "period")) %>%
    mutate(
      share = forestryCons / value,
      share = ifelse(share > 0.8, 0.6, share),
      share = ifelse(ef == "GDO", share, 0)
    ) %>%
    select(region, period, ef, share)

  shareForestry <- shareForestry %>%
    filter(ef != "BGDO") %>%
    rbind(mutate(filter(shareForestry, ef == "GDO"), ef = "BGDO"))

  forestry <- fuelCons %>%
    filter(flow == "AGRI_FOREST") %>%
    left_join(shareForestry, by = c("region", "period", "ef"), relationship = "many-to-many") %>%
    mutate(
      flow = "FORESTRY",
      share = ifelse(is.na(share), 0, share),
      value = ifelse(ef %in% c("GDO", "BGDO"), share * value, 0),
      unit = "Mtoe"
    ) %>%
    select(region, period, unit, flow, ef, value)

  agriculture <- fuelCons %>%
    filter(flow == "AGRI_FOREST") %>%
    left_join(shareForestry, by = c("region", "period", "ef"), relationship = "many-to-many") %>%
    mutate(
      flow = "AGRICULTURE",
      share = ifelse(is.na(share), 0, share),
      value = ifelse(ef %in% c("GDO", "BGDO"), (1 - share) * value, value)
    ) %>%
    select(region, period, flow, ef, value)

  # ---------------- Disaggregate agriculture to services ---------------
  shares <- getSharesAgricultureServices(data)
  sharesAverage <- getSharesAgricultureServices(dimSums(data, dim = 1)) %>%
    select(-region)


  shares <- crossing(
    region = unname(getISOlist()),
    variable = unique(shares$variable),
    ef = unique(shares$ef)
  ) %>%
    left_join(shares, by = c("region", "variable", "ef")) %>%
    left_join(sharesAverage, by = c("variable", "ef")) %>%
    mutate(
      share = ifelse(is.na(share.x), share.y, share.x),
      share = ifelse(is.na(share), 0, share)
    ) %>%
    select(region, variable, ef, share)

  agriculture <- agriculture %>%
    inner_join(shares, by = c("region", "ef"), relationship = "many-to-many") %>%
    mutate(value = ifelse(flow == "AGRICULTURE", value * share, value)) %>%
    select(region, period, variable, ef, value) %>%
    rename(flow = variable) %>%
    mutate(
      flow = toupper(flow),
      unit = "Mtoe"
    )

  final <- fuelCons %>%
    filter(flow == "FISHING") %>%
    rbind(forestry, agriculture) %>%
    as.quitte()

  final <- as.magpie(final)
  list(
    x = final,
    weight = NULL,
    unit = "Mtoe",
    description = "IEA; AGENRES; Disaggregate Agriculture"
  )
}

# Helpers ------------------------------------------------
getSharesAgricultureServices <- function(data) {
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
      "On farm transportation", "Rest", "Heating", "Irrigation"
    ),
    Fuel = c(rep("ELC", 9), "Thermal", "Thermal", rep("GDO", 7), "ELC", "Thermal", "GSL")
  )
  y <- toolAggregate(x, rel = mappingFuel, from = "Type", to = "Fuel", dim = 3.2, partrel = FALSE)

  # This is the disaggregation shares of services for each EF (e.g., Thermal -> Greenhouses, Irrigation)
  z <- as.quitte(y) %>%
    rename(ef = type) %>%
    group_by(region, period, ef) %>%
    mutate(share = value / sum(value)) %>%
    ungroup() %>%
    select(region, variable, ef, share)

  # Take the same shares for the biofuels, disaggregate the "Thermal" fuel
  shares <- z %>%
    bind_rows(
      z %>%
        filter(ef %in% c("GDO", "GSL")) %>%
        mutate(ef = paste0("B", ef))
    ) %>%
    bind_rows(
      z %>%
        filter(ef == "Thermal") %>%
        rename(type_old = ef) %>%
        select(-type_old) %>%
        tidyr::crossing(ef = c("LGN", "HCL", "NGS", "BMSWAS", "LPG", "BGAS", "STE", "GEO", "SOL", "RFO", "OGS"))
    ) %>%
    filter(ef != "Thermal")
  return(shares)
}
