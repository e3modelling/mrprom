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
    dataIEA_EV
    ) %>%
    rename(value = share) %>%
    as.quitte() %>%
    interpolate_missing_periods(period = seq(2021, 2030, 1)) %>%
    replace_na(list(value = 0)) %>%
    as.magpie() %>%
    toolAggregate(dim = 1, rel = countriesMappingProj, from = "RegionCode", to = "CountryCode")


  PRIMES <- sharesPrimes %>%
    group_by(region, tech) %>%
    mutate(new = value / value[period == 2030])
  
  PRIMES <- as.quitte(PRIMES)
  PRIMES <- select(PRIMES,-"variable")
  names(PRIMES) <- sub("tech","variable",names(PRIMES))
  PRIMES <- select(PRIMES,-"value")
  names(PRIMES) <- sub("new","value",names(PRIMES))
  PRIMES <- PRIMES %>% ungroup()
  PRIMES <- as.quitte(PRIMES)
  PRIMES <- as.magpie(PRIMES)
  IEA <- shareEVs
  PRIMES <- PRIMES[,,getItems(IEA,3)]
  PRIMES <- as.quitte(PRIMES)
  IEA <- as.quitte(IEA)
  IEA <- select(IEA,-"variable")
  names(IEA) <- sub("tech","variable",names(IEA))
  IEA <- IEA %>% ungroup()
  IEA <- as.quitte(IEA)
  
  PRIMES_trend_after_2030 <- PRIMES %>%
    group_by(region, variable) %>%
    arrange(period, .by_group = TRUE) %>%
    # get value in 2030
    mutate(value_2030 = value[period == 2030]) %>%
    # compute share relative to 2030
    mutate(value = value / value_2030) %>%
    # keep only periods after 2030
    filter(period > 2030) %>%
    ungroup() %>%
    select(-value_2030)
  
  IEA_2030 <- IEA %>%
    filter(period == 2030) %>%
    select(model, scenario, region, variable, unit, value_IEA_2030 = value)
  
  # Join with PRIMES_trend_after_2030
  qx_after_2030 <- PRIMES_trend_after_2030 %>%
    left_join(IEA_2030, by = c("model","scenario","region","variable","unit")) %>%
    # multiply the PRIMES ratio by 2030 value
    mutate(val = value * value_IEA_2030) %>%
    select(-value_IEA_2030)
  
  qx_after_2030 <- select(qx_after_2030,-value)
  names(qx_after_2030) <- sub("val","value",names(qx_after_2030))
  
  qx <- full_join(IEA, qx_after_2030, by = c("model","scenario","region", "variable", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))


  
}
