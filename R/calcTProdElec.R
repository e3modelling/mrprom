#' calcTProdElec
#'
#' Use ProdElec to generate targets for ProdElec
#' 
#' Info:
#' Ember: ProdElec data, shares for data that is missing from ENERDATA, until 2024
#' Primes: ProdElec data, shares for data that is missing from ENERDATA, EU countries until 2070,multiply by IEA trends(after 2070).
#' IEA: ProdElec data, find trends for ProdElec for each year.
#' The trends are the same for each country depending to the region. For example
#' HKG and CHN have the same trends for ProdElec
#' Shares for data that is missing from ENERDATA, 225 countries until 2050.
#' #' IEA mapping: "Africa" = "SSA", "Middle East" = "MEA", "Eurasia" = "REF",
#' "Southeast Asia" = "OAS", "Central and South America" = "LAM",
#' "Asia Pacific" = "CAZ", "Europe" = "NEU", "European Union" = "ELL"
#' calculate CAZ, NEU and ELL 
#' "CAZ" <- "CAZ" -  "OAS"
#' ELL and NEU have the same trends
#' IEA_non_EU <- "NEU" - "ELL"
#' "NEU" <- IEA_non_EU
#' "ELL" <- IEA_non_EU
#' The trends are multiplied with the historical from EMBER data to find the ProdElec.
#'
#' @return magpie object
#'
#' @author Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "TProdElec", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter %>% mutate select full_join arrange group_by distinct intersect setdiff ungroup group_map 
#' @importFrom tidyr pivot_wider

calcTProdElec <- function() {
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  historical <- getEmberProdElec() %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value")) %>%
    filter(period >= 2020)
  
  future <- getPrimesProdElec() %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value")) %>%
    filter(period >= 2025)
  
  # future_Nav <- getNavigateElecProd() %>%
  #   as.quitte() %>%
  #   select(c("region", "variable", "period", "value")) %>%
  #   filter(period >= 2025)
  
  future_IEA <- getIEAProdElec(historical) %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value")) %>%
    filter(period >= 2025)
  
  future <- full_join(future, future_IEA, by = c("region", "period", "variable")) %>%
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
    description = "EMBER,PRIMES,NAVIGATE; New power generation shares"
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

getEmberProdElec <- function() {
  power <- readSource("EMBER", convert = TRUE)
  power <- power[, , "Electricity generation"]
  power <- collapseDim(power, 3.3)
  
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
  power <- toolAggregate(power, dim = 3.1, rel = mapEMBER, from = "EMBER", to = "OPEN_PROM")
  
  ATHLGN <- power[, , "ATHCOAL"]
  getItems(ATHLGN, 3.1) <- "ATHLGN"
  PGCSP <- power[, , "PGSOL"]
  getItems(PGCSP, 3.1) <- "PGCSP"
  PGSHYD <- power[, , "PGLHYD"]
  getItems(PGSHYD, 3.1) <- "PGSHYD"
  PGAWNO <- power[, , "PGAWND"]
  getItems(PGAWNO, 3.1) <- "PGAWNO"
  
  power <- mbind(power, ATHLGN, PGCSP, PGSHYD, PGAWNO)
  
  data <- calcOutput(type = "IDataElecProd", mode = "NonCHP", aggregate = FALSE) / 1000
  
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  take_shares <- data
  
  take_shares <- as.quitte(take_shares) %>%
    interpolate_missing_periods(period = seq(2010, 2024, 1), expand.values = TRUE) %>%
    select(c("region", "period", "variable", "value"))
  
  techProd_data <- as.quitte(power)
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

getNavigateElecProd <- function() {
  # Navigate data
  x <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = FALSE)
  
  vars_cap <- data.frame(
    Navigate = c(
      "Secondary Energy|Electricity|Biomass", "Secondary Energy|Electricity|Coal",
      "Secondary Energy|Electricity|Gas", "Secondary Energy|Electricity|Hydro",
      "Secondary Energy|Electricity|Nuclear", "Secondary Energy|Electricity|Oil",
      "Secondary Energy|Electricity|Solar|CSP", "Secondary Energy|Electricity|Solar|PV",
      "Secondary Energy|Electricity|Wind|Offshore", "Secondary Energy|Electricity|Wind|Onshore",
      "Secondary Energy|Electricity|Geothermal"
    ),
    OPEN_PROM = c(
      "ATHBMSWAS", "ATHCOAL", "ATHGAS", "PGLHYD", "PGANUC", "ATHOIL",
      "PGCSP", "PGSOL", "PGAWNO", "PGAWND", "PGOTHREN"
    )
  )
  
  x <- x[, , vars_cap[, "Navigate"]][, , "REMIND-MAgPIE 3_2-4_6"]
  
  x <- as.quitte(x)
  
  suppressWarnings({
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
  })
  
  x <- filter(x, !is.na(x[["region"]]))
  x <- filter(x, !is.na(x[["value"]]))
  x <- distinct(x)
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  vars_cap <- filter(vars_cap, Navigate %in% getItems(x,3.3))
  
  power <- toolAggregate(x[, , as.character(unique(vars_cap[["Navigate"]]))], dim = 3.3, rel = vars_cap, from = "Navigate", to = "OPEN_PROM")
  
  power <- collapseDim(power, 3.1)
  power <- collapseDim(power, 3.1)
  
  power <- power * 277.778 # EJ to TWh
  
  getItems(power, 3.2) <- "TWh"
  
  ATHLGN <- power[, , "ATHCOAL"]
  getItems(ATHLGN, 3.1) <- "ATHLGN"
  PGSHYD <- power[, , "PGLHYD"]
  getItems(PGSHYD, 3.1) <- "PGSHYD"
  
  power <- mbind(power, ATHLGN, PGSHYD)
  
  data <- calcOutput(type = "IDataElecProd", mode = "NonCHP", aggregate = FALSE) / 1000
  
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  take_shares <- data
  
  take_shares <- as.quitte(take_shares) %>%
    interpolate_missing_periods(period = seq(2010, 2024, 1), expand.values = TRUE) %>%
    select(c("region", "period", "variable", "value"))
  
  techProd_data <- as.quitte(power)
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
  
  a <- add_dimension(a, dim = 3.2, add = "unit", nm = "TWh") %>%
    as.quitte() %>%
    interpolate_missing_periods(period = fStartHorizon:2100, expand.values = TRUE) %>%
    as.quitte() %>%
    as.magpie()
  
  # set NA to 0
  a[is.na(a)] <- 10^-6
  a <- a[, fStartHorizon:2100, ]
  return(a)
}

getPrimesProdElec <- function() {
  # Primes data
  a <- readSource("PrimesPGData", subtype = "power generation")
  
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
  
  power <- toolAggregate(a[, , as.character(unique(mapping[["primes"]]))], dim = 3.2, rel = mapping, from = "primes", to = "openprom")
  
  power <- collapseDim(power, 3.1)
  
  ATHLGN <- power[, , "ATHCOAL"]
  getItems(ATHLGN, 3.1) <- "ATHLGN"
  PGCSP <- power[, , "PGSOL"]
  getItems(PGCSP, 3.1) <- "PGCSP"
  
  power <- mbind(power, ATHLGN, PGCSP)
  
  power <- power[getRegions(power)[getRegions(power) %in% as.character(getISOlist())], , ]
  
  power <- as.quitte(power) %>% as.magpie()
  
  data <- calcOutput(type = "IDataElecProd", mode = "NonCHP", aggregate = FALSE) / 1000
  
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  take_shares <- data
  
  take_shares <- as.quitte(take_shares) %>%
    interpolate_missing_periods(period = seq(2010, 2024, 1), expand.values = TRUE) %>%
    select(c("region", "period", "variable", "value"))
  
  techProd_data <- as.quitte(power) %>%
    interpolate_missing_periods(period = seq(2000, 2100, 1), expand.values = TRUE) %>%
    select(c("region", "period", "variable", "value"))
  
  shares <- Reduce(
    function(x, y) full_join(x, y, by = c("region", "period", "variable")),
    list(
      getSharesTech(take_shares, techProd_data, c("PGSOL", "PGCSP")),
      getSharesTech(take_shares, techProd_data, c("ATHCOAL", "ATHLGN"))
    )
  ) %>%
    mutate(value = coalesce(value.x, value.y)) %>%
    select(region, period, variable, value)
  
  # Joins two datasets (techProd_data and shares) on region-variable-period.
  # Orders and groups the data by region and variable.
  # Fills missing value.y values using the growth rate of value.x over time.
  # Combines value.x and the (filled) value.y into a final unified column value.
  
  techProd <- techProd_data %>%
    left_join(shares, by = c("region", "variable", "period")) %>%
    arrange(region, variable, period) %>%
    group_by(region, variable) %>%
    mutate(value.y = {
      # Work on a copy
      filled <- value.y
      for (i in seq_along(filled)) {
        if (is.na(filled[i]) && i > 1 && !is.na(filled[i-1])) {
          filled[i] <- filled[i-1] * (value.x[i] / value.x[i-1])
        }
      }
      filled
    }) %>% ungroup()  %>%
    mutate(value = ifelse(is.na(value.y), value.x, value.y)) %>%
    select(c("region", "period", "variable", "value"))
  
  a <- as.quitte(techProd) %>% as.magpie()
  
  a <- add_dimension(a, dim = 3.2, add = "unit", nm = "TWh") %>%
    as.quitte() %>%
    interpolate_missing_periods(period = fStartHorizon:2100, expand.values = TRUE) %>%
    as.quitte() %>%
    as.magpie()
  
  # set NA to 0
  a[is.na(a)] <- 10^-6
  a <- a[, fStartHorizon:2100, ]
  
  ###Multiply Primes after 2070 with trends from IEA
  IEA_WEO_2025 <- readSource("IEA_WEO_2025_ExtendedData", subtype = "IEA_WEO_2025_ExtendedData")
  max_IEA_years <- max(getYears(IEA_WEO_2025, as.integer = TRUE))
  IEA_WEO_2025 <- IEA_WEO_2025[,,"Electricity generation"][,,"Stated Policies Scenario"][,,"TWh"]
  IEA_WEO_2025 <- collapseDim(IEA_WEO_2025,3.1)
  IEA_WEO_2025 <- collapseDim(IEA_WEO_2025,3.1)
  IEA_WEO_2025 <- collapseDim(IEA_WEO_2025,3.4)
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  map_IEA_WEO_2025_fuels <- data.frame(
    IEA = c(
      "Solar PV", "Wind",
      "Hydro", "Modern bioenergy and renewable waste",
      "Nuclear", "Coal: unabated",
      "Natural gas: unabated", "Oil", "Renewables"),
    OPEN_PROM = c(
      "PGSOL", "PGAWND", "PGLHYD", "ATHBMSWAS", "PGANUC", "ATHCOAL",
      "ATHGAS", "ATHOIL", "PGOTHREN"))
  
  IEA_WEO_2025 <- toolAggregate(IEA_WEO_2025[, , as.character(unique(map_IEA_WEO_2025_fuels[["IEA"]]))], dim = 3.3, rel = map_IEA_WEO_2025_fuels, from = "IEA", to = "OPEN_PROM")
  
  IEA_WEO_2025 <- IEA_WEO_2025["European Union",,]
  
  IEA_WEO_2025 <- as.quitte(IEA_WEO_2025)
  
  IEA_WEO_2025[["region"]] <- toolCountry2isocode((IEA_WEO_2025[["region"]]), mapping =
                                                    c("European Union" = "DEU"))
  
  IEA_WEO_2025 <- as.quitte(IEA_WEO_2025)
  IEA_WEO_2025 <- as.magpie(IEA_WEO_2025)
  
  IEA_WEO_2025 <- toolCountryFill(IEA_WEO_2025, fill = NA)
  IEA_WEO_2025[setdiff(getISOlist(),"DEU"),,] <- IEA_WEO_2025["DEU",,]
  
  IEA_WEO_2025 <-   as.quitte(IEA_WEO_2025) %>%
    interpolate_missing_periods(period = seq(2010, 2100, 1), expand.values = TRUE) %>%
    select(c("region", "period", "product", "value"))
  
  IEA <- as.quitte(IEA_WEO_2025) %>%
    arrange(region, product, period) %>%   # Sort by region, product, and period
    group_by(region, product) %>%          # Group by region and product
    mutate(
      prev_value = lag(value),
      diff_ratio = (value - prev_value) / if_else(prev_value == 0, 1, prev_value)
    ) %>%
    ungroup()
  
  IEA <- select(IEA,"region","product","unit","period","diff_ratio","product")
  
  names(IEA) <- sub("diff_ratio", "value", names(IEA))
  
  #set trend equal to 2050 after this year
  IEA <- IEA %>%
    group_by(region, product) %>%
    mutate(
      value_2050 = value[period == 2050][1],  # grab value for 2050 within each region-product group
      value = ifelse(period > 2050, value_2050, value)
    ) %>%
    select(-value_2050) %>%
    ungroup()
  
  IEA <- as.quitte(IEA) %>% as.magpie()
  
  #2010 is NA and set equal to 2011
  IEA[,2010,] <- IEA[,2011,]
  
  ATHLGN <- IEA[, , "ATHCOAL"]
  getItems(ATHLGN, 3) <- "ATHLGN"
  PGCSP <- IEA[, , "PGSOL"]
  getItems(PGCSP, 3) <- "PGCSP"
  PGAWNO <- IEA[, , "PGAWND"]
  getItems(PGAWNO, 3) <- "PGAWNO"
  PGSHYD <- IEA[, , "PGLHYD"]
  getItems(PGSHYD, 3) <- "PGSHYD"
  
  IEA <- mbind(IEA,PGCSP,ATHLGN,PGAWNO,PGSHYD)
  
  data <- calcOutput(type = "IDataElecProd", mode = "NonCHP", aggregate = FALSE) / 1000
  
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  take_shares <- data
  
  take_shares <- as.quitte(take_shares) %>%
    interpolate_missing_periods(period = seq(2010, 2024, 1), expand.values = TRUE) %>%
    select(c("region", "period", "variable", "value"))
  
  techProd_data <- as.quitte(IEA)
  techProd_data <- select(techProd_data, c("region", "product", "period", "value"))
  names(techProd_data) <- sub("product", "variable", names(techProd_data))
  
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
  techProd <- as.quitte(techProd)
  
  historical <- a
  historical <- collapseDim(historical,3.2)
  
  techProd <- filter(techProd,region %in% getItems(historical,1))
  
  techProd<-as.quitte(techProd)
  techProd<-as.magpie(techProd)
  
  qa <- as.quitte(historical)
  qx <- as.quitte(techProd)
  
  df <- qa %>%
    left_join(qx, by = c("model","scenario","region","variable","unit","period"))
  
  names(df) <- sub("value.x", "value", names(df))
  names(df) <- sub("value.y", "multiplier", names(df))
  
  df <- filter(df, period > 2059)
  
  # If a value.y is missing for a given period,
  # it looks at the previous period’s value.y and adjusts
  # it by the same growth rate that happened in value.x
  
  df_updated <- df %>%
    group_by(region, variable) %>%
    arrange(period) %>%
    group_modify(~ {
      d <- .x
      start <- which(!is.na(d$value))[1]
      if (!is.na(start)) {
        for (i in (start + 1):nrow(d)) {
          d$value[i] <- d$value[i - 1] + d$value[i - 1] * d$multiplier[i - 1]
        }
      }
      d
    }) %>%
    ungroup()
  
  df_updated <- select(df_updated, "region","model","scenario","period","value","variable")
  
  a <- as.quitte(filter(df_updated,period>2019)) %>% as.magpie()
  
  a <- add_dimension(a, dim = 3.2, add = "unit", nm = "TWh")
  historical <- add_dimension(historical, dim = 3.2, add = "unit", nm = "TWh")
  
  a <- mbind(historical[,setdiff(getYears(historical),getYears(a)),], a)
  
  suppressMessages(
    suppressWarnings(
      a <- toolCountryFill(a, fill = NA)
    )
  )
  
  # set NA to 0
  a[is.na(a)] <- 10^-6
  
  return(a)
}

getIEAProdElec <- function(historical) {
  
  ###Multiply Primes after 2070 with trends from IEA
  IEA_WEO_2025 <- readSource("IEA_WEO_2025_ExtendedData", subtype = "IEA_WEO_2025_ExtendedData")
  max_IEA_years <- max(getYears(IEA_WEO_2025, as.integer = TRUE))
  IEA_WEO_2025 <- IEA_WEO_2025[,,"Electricity generation"][,,"Stated Policies Scenario"][,,"TWh"]
  IEA_WEO_2025 <- collapseDim(IEA_WEO_2025,3.1)
  IEA_WEO_2025 <- collapseDim(IEA_WEO_2025,3.1)
  IEA_WEO_2025 <- collapseDim(IEA_WEO_2025,3.4)
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  map_IEA_WEO_2025_fuels <- data.frame(
    IEA = c(
      "Solar PV", "Wind",
      "Hydro", "Modern bioenergy and renewable waste",
      "Nuclear", "Coal: unabated",
      "Natural gas: unabated", "Oil", "Renewables"),
    OPEN_PROM = c(
      "PGSOL", "PGAWND", "PGLHYD", "ATHBMSWAS", "PGANUC", "ATHCOAL",
      "ATHGAS", "ATHOIL", "PGOTHREN"))
  
  IEA_WEO_2025 <- toolAggregate(IEA_WEO_2025[, , as.character(unique(map_IEA_WEO_2025_fuels[["IEA"]]))], dim = 3.3, rel = map_IEA_WEO_2025_fuels, from = "IEA", to = "OPEN_PROM")
  
  IEA_WEO_2025 <- as.quitte(IEA_WEO_2025)
  
  IEA_WEO_2025[["region"]] <- toolCountry2isocode((IEA_WEO_2025[["region"]]), mapping =
                                                    c("Africa" = "SSA",
                                                      "Middle East" = "MEA",
                                                      "Eurasia" = "REF",
                                                      "Southeast Asia" = "OAS",
                                                      "Central and South America" = "LAM",
                                                      "Asia Pacific" = "CAZ",
                                                      "Europe" = "NEU",
                                                      "European Union" = "ELL"))
  
  IEA_WEO_2025 <- filter(IEA_WEO_2025, !is.na(IEA_WEO_2025[["region"]]))
  IEA_WEO_2025 <- filter(IEA_WEO_2025, !is.na(IEA_WEO_2025[["value"]]))
  IEA_WEO_2025 <- distinct(IEA_WEO_2025)
  IEA_WEO_2025 <- as.quitte(IEA_WEO_2025) %>% 
    interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
  
  IEA_WEO_2025 <- as.quitte(IEA_WEO_2025) %>% as.magpie()
  
  #calculate CAZ,NEU and ELL value
  IEA_WEO_2025["CAZ",,] <- IEA_WEO_2025["CAZ",,] - IEA_WEO_2025["OAS",,]
  
  #ELL and NEU have the same trends
  IEA_non_EU <- IEA_WEO_2025["NEU",,] - IEA_WEO_2025["ELL",,]
  IEA_WEO_2025["NEU",,] <- IEA_non_EU
  
  map <- toolGetMapping("regionmappingOPDEV5.csv", "regional", where = "mrprom")
  
  map_IEA <- filter(map, Region.Code %in% getRegions(IEA_WEO_2025))
  
  #set SE of countries equal to their regions
  IEA <-  toolAggregate(IEA_WEO_2025[unique(map_IEA[,"Region.Code"]),,], rel = map_IEA,  from = "Region.Code", to = "ISO3.Code", weight = NULL)
  
  #calculate region CHA
  IEA_CHA <- IEA_WEO_2025["CHN",,]
  
  IEA_CHA <- add_columns(IEA_CHA, addnm = "HKG", dim = 1, fill = NA)
  IEA_CHA <- add_columns(IEA_CHA, addnm = "MAC", dim = 1, fill = NA)
  IEA_CHA <- add_columns(IEA_CHA, addnm = "TWN", dim = 1, fill = NA)
  
  IEA_CHA["HKG",,] <- IEA_CHA["CHN",,]
  IEA_CHA["MAC",,] <- IEA_CHA["CHN",,]
  IEA_CHA["TWN",,] <- IEA_CHA["CHN",,]
  
  IEA <- mbind(IEA, IEA_CHA)
  
  #find trend, period-to-period relative change (growth rate)
  IEA <- as.quitte(IEA) %>%
    arrange(region, product, period) %>%   # Sort by region, product, and period
    group_by(region, product) %>%          # Group by region and product
    mutate(
      prev_value = lag(value),
      diff_ratio = (value - prev_value) / if_else(prev_value == 0, 1, prev_value)
    ) %>%
    ungroup()
  
  IEA <- select(IEA,"region","variable","unit","period","diff_ratio","product")
  
  names(IEA) <- sub("diff_ratio", "value", names(IEA))
  
  #set trend equal to 2050 after this year
  IEA <- IEA %>%
    group_by(region, product) %>%
    mutate(
      value_2050 = value[period == 2050][1],  # grab value for 2050 within each region-product group
      value = ifelse(period > 2050, value_2050, value)
    ) %>%
    select(-value_2050) %>%
    ungroup()
  
  IEA <- as.quitte(IEA) %>% as.magpie()
  
  #for SSA countries put trend HYDRO equal to zero after 2050
  #IEA[map[map[,"Region.Code"] == "SSA",2],,][,,"PGLHYD"][,getYears(IEA, as.integer = TRUE)[getYears(IEA, as.integer = TRUE) > 2050],]<- 0.01
  
  #2010 is NA and set equal to 2011
  IEA[,2010,] <- IEA[,2011,]
  
  ATHLGN <- IEA[, , "ATHCOAL"]
  getItems(ATHLGN, 3.3) <- "ATHLGN"
  PGCSP <- IEA[, , "PGSOL"]
  getItems(PGCSP, 3.3) <- "PGCSP"
  PGAWNO <- IEA[, , "PGAWND"]
  getItems(PGAWNO, 3.3) <- "PGAWNO"
  PGSHYD <- IEA[, , "PGLHYD"]
  getItems(PGSHYD, 3.3) <- "PGSHYD"
  
  IEA <- mbind(IEA,PGCSP,ATHLGN,PGAWNO,PGSHYD)
  
  data <- calcOutput(type = "IDataElecProd", mode = "NonCHP", aggregate = FALSE) / 1000
  
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  take_shares <- data
  
  take_shares <- as.quitte(take_shares) %>%
    interpolate_missing_periods(period = seq(2010, 2024, 1), expand.values = TRUE) %>%
    select(c("region", "period", "variable", "value"))
  
  techProd_data <- as.quitte(IEA)
  techProd_data <- select(techProd_data, c("region", "product", "period", "value"))
  names(techProd_data) <- sub("product", "variable", names(techProd_data))
  
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
  
  historical <- filter(historical,region %in% getItems(techProd,1))
  
  historical<-as.quitte(historical)
  historical<-as.magpie(historical)
  
  years <- setdiff(getYears(techProd), getYears(historical))
  
  years_int <- setdiff(getYears(techProd, as.integer = TRUE), getYears(historical, as.integer = TRUE))
  
  historical <- add_columns(historical, addnm = years, dim = 2, fill = NA)
  
  #put trend HYDRO equal to 0.01 after 2050
  techProd[map[map[,"Region.Code"] == "SSA",2],,][,,"PGLHYD"][,getYears(IEA, as.integer = TRUE)[getYears(IEA, as.integer = TRUE) > 2050],]<- 0.01
  techProd[map[map[,"Region.Code"] == "SSA",2],,][,,"PGSHYD"][,getYears(IEA, as.integer = TRUE)[getYears(IEA, as.integer = TRUE) > 2050],]<- 0.01

  qa <- as.quitte(historical)
  qx <- as.quitte(techProd)
  
  df <- qa %>%
    left_join(qx, by = c("model","scenario","region","variable","unit","period"))
  
  names(df) <- sub("value.x", "value", names(df))
  names(df) <- sub("value.y", "multiplier", names(df))
  
  #projecting or growing values forward
  df_updated <- df %>%
    group_by(region, variable) %>%
    arrange(period) %>%
    group_modify(~ {
      d <- .x
      start <- which(!is.na(d$value))[1]
      if (!is.na(start)) {
        for (i in (start + 1):nrow(d)) {
          d$value[i] <- d$value[i - 1] + d$value[i - 1] * d$multiplier[i - 1]
        }
      }
      d
    }) %>%
    ungroup()
  
  df_updated <- select(df_updated, "region","model","scenario","period","value","variable")
  
  a <- as.quitte(filter(df_updated,period>2019)) %>% as.magpie()
  
  a <- add_dimension(a, dim = 3.2, add = "unit", nm = "GW")
  
  # set NA to 0
  a[is.na(a)] <- 10^-6
  
  return(a)
}
