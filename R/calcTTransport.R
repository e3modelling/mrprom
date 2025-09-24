#' calcTTransport
#'
#' Use Primes for
#'
#' @author Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "TTransport", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select full_join arrange group_by distinct intersect setdiff ungroup group_map
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom quitte as.quitte interpolate_missing_periods

calcTTansport <- function() {
  technologyMapping <- list(
    "CNG" = "NGS",
    "Diesel Conventional" = "GDO",
    "Diesel Hybrid" = "CHEVGDO",
    "Diesel plug-in hybrid" = "PHEVGDO",
    "E85" = "MET",
    "Electric" = "ELC",
    "Gasoline Conventional" = "GSL",
    "Gasoline Hybrid" = "CHEVGSL",
    "Gasoline plug-in hybrid" = "PHEVGSL",
    "Hydrogen" = "H2F",
    "LPG" = "LPG"
  )
  mappingEVs <- list(
    "BEV" = "ELC",
    "PHEVGDO" = "PHEVGDO",
    "PHEVGSL" = "PHEVGSL",
    "FCEV" = "H2F"
  )
  countriesMappingProj <- toolGetMapping("regionmappingH12.csv", where = "madrat") %>%
    mutate(
      RegionCode = ifelse(RegionCode %in% c("CHA", "IND", "EUR", "USA"), RegionCode, "RWRL")
    )


  sharesPrimes <- readSource("PrimesNewTransport", subtype = "Stock") %>%
    as.quitte() %>%
    filter(period >= 2020, sector == "PC", variable != "AVIATION") %>%
    interpolate_missing_periods(period = 2020:2100, expand.values = TRUE) %>%
    select(region, period, variable, value) %>%
    rename(tech = variable) %>%
    group_by(region, period) %>%
    mutate(
      total = sum(value),
      value = value / total,
      tech = recode(tech, !!!technologyMapping)
    ) %>%
    select(-total) %>%
    ungroup()


  # %>%
  # pivot_wider(names_from = tech) %>%
  # write.csv("TargetShares.csv", row.names = FALSE)


  # SFC <- calcOutput("ISFC", aggregate = FALSE, subtype = "projection") %>%
  #  as.quitte()

  dataIEA_EV <- readSource("IEA_EV", convert = FALSE) %>%
    as.quitte() %>%
    filter(
      parameter %in% c("EV stock", "EV stock share"),
      category == "Projection-STEPS",
      !is.na(value),
      period >= 2021,
      variable == "Cars",
      region %in% c("RWRL", "USA", "CHN", "IND", "EUR")
    ) %>%
    mutate(region = if_else(region == "CHN", "CHA", region))

  # countriesMappingProj
  shareEVs <- helperGetEVShares(
    mappingEVs,
    dataIEA_EV,
    finalY = 2030,
    fillRegions = FALSE
  ) %>%
    rename(value = share) %>%
    as.quitte() %>%
    interpolate_missing_periods(period = seq(2021, 2030, 1)) %>%
    replace_na(list(value = 0)) %>%
    as.magpie() %>%
    toolAggregate(dim = 1, rel = countriesMappingProj, from = "RegionCode", to = "CountryCode")


  sharesPrimes %>%
    group_by(region, tech) %>%
    mutate(new = value / value[period == 2020])
}
