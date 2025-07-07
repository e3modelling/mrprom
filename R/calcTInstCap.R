#' calcTInstCap
#'
#' Use capacities to generate targets for capacity.
#'
#' @return magpie object
#'
#' @author Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "TInstCap", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr select group_by arrange mutate ungroup unique
#' @importFrom tidyr pivot_wider

calcTInstCap <- function() {
  historical <- getEmberCap() %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value")) %>%
    filter(period >= 2020)

  future <- getPrimesCap() %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value")) %>%
    filter(period >= 2025)

  future_Nav <- getNavigateCap() %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value")) %>%
    filter(period >= 2025)

  future <- full_join(future, future_Nav, by = c("region", "period", "variable")) %>%
    mutate(value = ifelse((value.x == 10^-6 & !(is.na(value.y))), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))

  x <- historical %>%
    full_join(future, by = c("region", "variable", "period")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c(value.x, value.y)) %>%
    as.quitte() %>%
    as.magpie()

  list(
    x = x,
    weight = NULL,
    unit = "ratio",
    description = "EMBER,PRIMES,NAVIGATE; New capacity shares"
  )
}

# Helpers---------------------------------------------------
getSharesTech <- function(take_shares, techProd_data, vars) {
  shares <- take_shares %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value")) %>%
    filter(variable %in% vars) %>%
    group_by(region, period) %>% # Group by region and period
    mutate(total_value = sum(value), share = value / total_value) %>%
    select(c("region", "variable", "period", "share")) %>%
    replace_na(list(share = 0)) %>%
    right_join(techProd_data, by = c("region", "period", "variable")) %>%
    mutate(value = value * share) %>%
    select(-share) %>%
    ungroup()
  return(shares)
}

getEmberCap <- function() {
  capacities <- readSource("EMBER", convert = TRUE)
  capacities <- capacities[, , "Capacity"]
  capacities <- collapseDim(capacities, 3.3)

  mapEMBER <- data.frame(
    EMBER = c(
      "Bioenergy", "Coal", "Gas", "Hydro", "Nuclear", "Other Fossil",
      "Other Renewables", "Solar", "Wind"
    ),
    OPEN_PROM = c(
      "ATHBMSWAS", "ATHCOAL", "ATHGAS", "PGLHYD", "PGANUC", "ATHOIL",
      "PGOTHREN", "PGSOL", "PGAWND"
    )
  )

  # aggregate from ENERDATA fuels to reporting fuel categories
  capacities <- toolAggregate(capacities, dim = 3.1, rel = mapEMBER, from = "EMBER", to = "OPEN_PROM")

  ATHLGN <- capacities[, , "ATHCOAL"]
  getItems(ATHLGN, 3.1) <- "ATHLGN"
  PGCSP <- capacities[, , "PGSOL"]
  getItems(PGCSP, 3.1) <- "PGCSP"
  PGSHYD <- capacities[, , "PGLHYD"]
  getItems(PGSHYD, 3.1) <- "PGSHYD"
  PGAWNO <- capacities[, , "PGAWND"]
  getItems(PGAWNO, 3.1) <- "PGAWNO"

  capacities <- mbind(capacities, ATHLGN, PGCSP, PGSHYD, PGAWNO)

  data <- readSource("ENERDATA", "capacity", convert = TRUE)
  data[is.na(data)] <- 0
  data[, , "Total electricity capacity coal, lignite (multifuel included)"] <- data[, , "Total electricity capacity coal, lignite (multifuel included)"] - data[, , "Single fired electricity capacity lignite"]

  data <- collapseDim(data, 3.4)
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  data <- as.quitte(data) %>%
    filter(period >= fStartHorizon & period <= 2021) %>%
    replace_na(list(value = 0))

  # load current OPENPROM set configuration
  sets <- toolGetMapping(
    name = "PGALL.csv",
    type = "blabla_export",
    where = "mrprom"
  )[, 1]

  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(
    name = "prom-enerdata-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    select(c("PROM_Code", "ENERDATA_Name")) %>%
    filter(PROM_Code %in% sets) %>%
    separate_rows(PROM_Code, sep = ",") %>%
    rename(product = ENERDATA_Name, variable = PROM_Code) %>%
    na.omit(map)

  names(data) <- sub("variable", "product", names(data))

  data <- filter(data, unit == "MW")

  # group by each technology and sum over its sub-technologies
  techProd <- data %>%
    left_join(map, by = "product", relationship = "many-to-many") %>%
    select(c("region", "period", "value", "variable")) %>%
    group_by(region, period, variable) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    drop_na()

  take_shares <- techProd

  take_shares <- as.quitte(take_shares) %>%
    interpolate_missing_periods(period = seq(2010, 2024, 1), expand.values = TRUE) %>%
    select(c("region", "period", "variable", "value"))

  techProd_data <- as.quitte(capacities)
  techProd_data <- select(techProd_data, c("region", "variable", "period", "value"))

  shares <- Reduce(
    function(x, y) full_join(x, y, by = c("region", "period", "variable")),
    list(
      getSharesTech(take_shares, techProd_data, c("PGSOL", "PGCSP")),
      getSharesTech(take_shares, techProd_data, c("PGLHYD", "PGSHYD")),
      getSharesTech(take_shares, techProd_data, c("PGAWND", "PGAWNO")),
      getSharesTech(take_shares, techProd_data, c("ATHCOAL", "ATHLGN"))
    )
  ) %>%
    mutate(value = coalesce(value.x, value.y, value.x.x, value.y.y)) %>%
    select(region, period, variable, value)

  techProd <- techProd_data %>%
    left_join(shares, by = c("region", "variable", "period")) %>%
    mutate(value = ifelse(is.na(value.y), value.x, value.y)) %>%
    select(c("region", "period", "variable", "value"))

  techProd <- as.quitte(techProd) %>% as.magpie()

  # Set NA to 0
  techProd[is.na(techProd)] <- 0
  return(techProd)
}

getNavigateCap <- function() {
  # Navigate data
  x <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = FALSE)

  vars_cap <- data.frame(
    Navigate = c(
      "Capacity|Electricity|Biomass", "Capacity|Electricity|Coal",
      "Capacity|Electricity|Gas", "Capacity|Electricity|Hydro",
      "Capacity|Electricity|Nuclear", "Capacity|Electricity|Oil",
      "Capacity|Electricity|Solar|CSP", "Capacity|Electricity|Solar|PV",
      "Capacity|Electricity|Wind|Offshore", "Capacity|Electricity|Wind|Onshore",
      "Capacity|Electricity|Geothermal"
    ),
    OPEN_PROM = c(
      "ATHBMSWAS", "ATHCOAL", "ATHGAS", "PGLHYD", "PGANUC", "ATHOIL",
      "PGCSP", "PGSOL", "PGAWNO", "PGAWND", "PGOTHREN"
    )
  )

  x <- x[, , vars_cap[, "Navigate"]][, , "REMIND-MAgPIE 3_2-4_6"]

  x <- as.quitte(x)

  x[["region"]] <- toolCountry2isocode((x[["region"]]),
    mapping =
      c(
        "R9CHINA" = "CHN",
        "R9INDIA" = "IND",
        "R9USA" = "USA",
        "REMIND 3_2|India" = "IND",
        "REMIND 3_2|Japan" = "JPN",
        "REMIND 3_2|United States of America" = "USA",
        "REMIND 3_2|Russia and Reforming Economies" = "RUS"
      )
  )
  x <- filter(x, !is.na(x[["region"]]))
  x <- filter(x, !is.na(x[["value"]]))
  x <- distinct(x)
  x <- as.quitte(x)
  x <- as.magpie(x)

  capacities <- toolAggregate(x[, , as.character(unique(vars_cap[["Navigate"]]))], dim = 3.3, rel = vars_cap, from = "Navigate", to = "OPEN_PROM")

  capacities <- collapseDim(capacities, 3.1)
  capacities <- collapseDim(capacities, 3.1)

  ATHLGN <- capacities[, , "ATHCOAL"]
  getItems(ATHLGN, 3.1) <- "ATHLGN"
  PGSHYD <- capacities[, , "PGLHYD"]
  getItems(PGSHYD, 3.1) <- "PGSHYD"

  capacities <- mbind(capacities, ATHLGN, PGSHYD)

  data <- readSource("ENERDATA", "capacity", convert = TRUE)
  data[is.na(data)] <- 0
  data[, , "Total electricity capacity coal, lignite (multifuel included)"] <- data[, , "Total electricity capacity coal, lignite (multifuel included)"] - data[, , "Single fired electricity capacity lignite"]

  data <- collapseDim(data, 3.4)
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  years <- getYears(data, as.integer = TRUE)
  data <- as.quitte(data) %>%
    filter(period >= fStartHorizon & period <= 2021) %>%
    replace_na(list(value = 0))

  # load current OPENPROM set configuration
  sets <- toolGetMapping(
    name = "PGALL.csv",
    type = "blabla_export",
    where = "mrprom"
  )[, 1]

  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(
    name = "prom-enerdata-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    select(c("PROM_Code", "ENERDATA_Name")) %>%
    filter(PROM_Code %in% sets) %>%
    separate_rows(PROM_Code, sep = ",") %>%
    rename(product = ENERDATA_Name, variable = PROM_Code) %>%
    na.omit(map)

  names(data) <- sub("variable", "product", names(data))

  data <- filter(data, unit == "MW")

  # group by each technology and sum over its sub-technologies
  techProd <- data %>%
    left_join(map, by = "product", relationship = "many-to-many") %>%
    select(c("region", "period", "value", "variable")) %>%
    group_by(region, period, variable) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    drop_na()

  take_shares <- techProd

  take_shares <- as.quitte(take_shares) %>%
    interpolate_missing_periods(period = min(getYears(capacities, as.integer = TRUE)):max(getYears(capacities, as.integer = TRUE)), expand.values = TRUE) %>%
    select(c("region", "period", "variable", "value"))

  techProd_data <- as.quitte(capacities)
  techProd_data <- select(techProd_data, c("region", "variable", "period", "value"))

  shares <- Reduce(
    function(x, y) full_join(x, y, by = c("region", "period", "variable")),
    list(
      getSharesTech(take_shares, techProd_data, c("PGLHYD", "PGSHYD")),
      getSharesTech(take_shares, techProd_data, c("ATHCOAL", "ATHLGN"))
    )
  ) %>%
    mutate(value = coalesce(value.x, value.y)) %>%
    select(region, period, variable, value)

  techProd <- techProd_data %>%
    left_join(shares, by = c("region", "variable", "period")) %>%
    mutate(value = ifelse(is.na(value.y), value.x, value.y)) %>%
    select(c("region", "period", "variable", "value"))

  a <- as.quitte(techProd) %>% as.magpie()

  a <- add_dimension(a, dim = 3.2, add = "unit", nm = "GW") %>%
    as.quitte() %>%
    interpolate_missing_periods(period = fStartHorizon:2100, expand.values = TRUE) %>%
    as.quitte() %>%
    as.magpie()

  # set NA to 0
  a[is.na(a)] <- 10^-6
  a <- a[, fStartHorizon:2100, ]
  return(a)
}



getPrimesCap <- function() {
  # Primes data
  a <- readSource("PrimesPGData", subtype = "capacity")

  mapping <- list(
    primes = c(
      "Nuclear energy", "Lakes", "Run of river", "Wind on-shore",
      "Wind off-shore", "Solar", "Solids fired", "Oil fired", "Gas fired",
      "Biomass-waste fired", "Geothermal heat"
    ),
    openprom = c(
      "PGANUC", "PGLHYD", "PGSHYD", "PGAWND",
      "PGAWNO", "PGSOL", "ATHCOAL", "ATHOIL", "ATHGAS",
      "ATHBMSWAS", "PGOTHREN"
    )
  )

  mapping <- as.data.frame(mapping)

  capacities <- toolAggregate(a[, , as.character(unique(mapping[["primes"]]))], dim = 3.2, rel = mapping, from = "primes", to = "openprom")

  capacities <- collapseDim(capacities, 3.1)

  ATHLGN <- capacities[, , "ATHCOAL"]
  getItems(ATHLGN, 3.1) <- "ATHLGN"
  PGCSP <- capacities[, , "PGSOL"]
  getItems(PGCSP, 3.1) <- "PGCSP"

  capacities <- mbind(capacities, ATHLGN, PGCSP)

  capacities <- capacities[getRegions(capacities)[getRegions(capacities) %in% as.character(getISOlist())], , ]
  capacities <- toolCountryFill(capacities, fill = NA)

  capacities <- as.quitte(capacities) %>% as.magpie()

  data <- readSource("ENERDATA", "capacity", convert = TRUE)
  data[is.na(data)] <- 0
  data[, , "Total electricity capacity coal, lignite (multifuel included)"] <- data[, , "Total electricity capacity coal, lignite (multifuel included)"] - data[, , "Single fired electricity capacity lignite"]

  data <- collapseDim(data, 3.4)
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  years <- getYears(data, as.integer = TRUE)
  data <- as.quitte(data) %>%
    filter(period >= fStartHorizon & period <= 2021) %>%
    replace_na(list(value = 0))

  # load current OPENPROM set configuration
  sets <- toolGetMapping(
    name = "PGALL.csv",
    type = "blabla_export",
    where = "mrprom"
  )[, 1]

  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(
    name = "prom-enerdata-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    select(c("PROM_Code", "ENERDATA_Name")) %>%
    filter(PROM_Code %in% sets) %>%
    separate_rows(PROM_Code, sep = ",") %>%
    rename(product = ENERDATA_Name, variable = PROM_Code) %>%
    na.omit(map)

  names(data) <- sub("variable", "product", names(data))

  data <- filter(data, unit == "MW")

  # group by each technology and sum over its sub-technologies
  techProd <- data %>%
    left_join(map, by = "product", relationship = "many-to-many") %>%
    select(c("region", "period", "value", "variable")) %>%
    group_by(region, period, variable) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    drop_na()

  take_shares <- techProd

  take_shares <- as.quitte(take_shares) %>%
    interpolate_missing_periods(period = min(getYears(capacities, as.integer = TRUE)):max(getYears(capacities, as.integer = TRUE)), expand.values = TRUE) %>%
    select(c("region", "period", "variable", "value"))

  techProd_data <- as.quitte(capacities)
  techProd_data <- select(techProd_data, c("region", "variable", "period", "value"))

  shares <- Reduce(
    function(x, y) full_join(x, y, by = c("region", "period", "variable")),
    list(
      getSharesTech(take_shares, techProd_data, c("PGSOL", "PGCSP")),
      getSharesTech(take_shares, techProd_data, c("ATHCOAL", "ATHLGN"))
    )
  ) %>%
    mutate(value = coalesce(value.x, value.y)) %>%
    select(region, period, variable, value)

  techProd <- techProd_data %>%
    left_join(shares, by = c("region", "variable", "period")) %>%
    mutate(value = ifelse(is.na(value.y), value.x, value.y)) %>%
    select(c("region", "period", "variable", "value"))

  a <- as.quitte(techProd) %>% as.magpie()

  a <- add_dimension(a, dim = 3.2, add = "unit", nm = "GW") %>%
    as.quitte() %>%
    interpolate_missing_periods(period = fStartHorizon:2100, expand.values = TRUE) %>%
    as.quitte() %>%
    as.magpie()

  # set NA to 0
  a[is.na(a)] <- 10^-6
  a <- a[, fStartHorizon:2100, ]
  return(a)
}
