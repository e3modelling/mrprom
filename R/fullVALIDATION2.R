#' fullVALIDATION2
#'
#' Read data from PRIMES and EMBER convert it to a mrprom mif
#' file so to compare output mif file with OPEN-PROM output.
#'
#' @return The mif file
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- retrieveData("fullVALIDATION2", regionmapping = "regionmappingOPDEV3.csv")
#' }
#'
#' @importFrom quitte as.quitte write.mif
#' @importFrom dplyr select filter %>% left_join mutate across group_by
#' @importFrom tidyr separate_rows separate_longer_delim separate_wider_delim drop_na
#' @importFrom stringr str_remove str_remove_all
#' @importFrom utils  read.csv
#' @export

fullVALIDATION2 <- function() {
  
  # Read regional mapping
  rmap <- toolGetMapping(getConfig("regionmapping"), "regional", where = "mrprom")
  
  horizon <-c(2010:2100)
  
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  ######### Primes_BALANCES
  Primes_BALANCES <- calcOutput(type = "Primes", aggregate = FALSE)
  
  years_in_horizon <-  horizon[horizon %in% getYears(Primes_BALANCES, as.integer = TRUE)]
  
  Primes_BALANCES <- collapseDim(Primes_BALANCES, 3.2)
  
  # OPEN-PROM sectors
  sector <- c("TRANSE", "INDSE", "DOMSE", "NENSE")
  sector_name <- c("Transportation", "Industry", "Residential and Commercial", "Non Energy and Bunkers")
  
  Primes_BALANCES_agg <- NULL
  for (y in 1 : length(sector)) {
    
    sets6 <- toolGetMapping(paste0(sector[y], ".csv"),
                            type = "blabla_export",
                            where = "mrprom")
    Primes_BALANCES_SECTORAL <- Primes_BALANCES[,,getItems(Primes_BALANCES,3.1)[getItems(Primes_BALANCES,3.1) %in% sets6[,1]]]
    getItems(Primes_BALANCES_SECTORAL, 3.1) <- paste0("Final Energy|", sector_name[y],"|", getItems(Primes_BALANCES_SECTORAL, 3.1))
    Primes_BALANCES_agg <- mbind(Primes_BALANCES_agg, Primes_BALANCES_SECTORAL)
  }
  
  # remove . from magpie object and replace with |
  Primes_BALANCES_agg <- as.quitte(Primes_BALANCES_agg)
  Primes_BALANCES_agg[[names(Primes_BALANCES_agg[, 4])]] <- paste0(Primes_BALANCES_agg[[names(Primes_BALANCES_agg[, 4])]], "|", Primes_BALANCES_agg[["new"]])
  Primes_BALANCES_agg <- select(Primes_BALANCES_agg, -c("new"))
  Primes_BALANCES_agg <- as.quitte(Primes_BALANCES_agg) %>% as.magpie()
  
  # write data in mif file
  write.report(Primes_BALANCES_agg[, years_in_horizon, ],file = "reporting.mif", model = "Primes_BALANCES", unit = "Mtoe", append = TRUE, scenario = "fullVALIDATION2")
  #####################
  
  #Primes Secondary Energy Electricity data
  PrimesSE <- readSource("PrimesPGData", subtype = "SE")
  
  PrimesSE <- PrimesSE[getRegions(PrimesSE)[getRegions(PrimesSE) %in% as.character(getISOlist())], , ]
  
  PrimesSE <-  as.quitte(PrimesSE) %>%
    interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
  
  PrimesSE <- as.quitte(PrimesSE) %>% as.magpie()
  
  PrimesSE <- PrimesSE[,fStartHorizon:max(getYears(PrimesSE, as.integer = TRUE)),]
  
  PrimesSE <- PrimesSE[intersect(getRegions(PrimesSE),rmap[,3]),,]
  PrimesSE <- collapseDim(PrimesSE, 3.1)
  
  years_in_horizon <-  horizon[horizon %in% getYears(PrimesSE, as.integer = TRUE)]
  
  # write data in mif file
  write.report(PrimesSE[, years_in_horizon, ],file = "reporting.mif", model = "PrimesSE", unit = "TWh", append = TRUE, scenario = "fullVALIDATION2")
  ###############
  
  ######### Ember Capacity
  
  rename_EF <- c(
    "LGN" = "Lignite",
    "HCL" = "Hard coal",
    "GDO" = "Oil",
    "NGS" = "Gas",
    "BMSWAS" = "Biomass",
    "NUC" = "Nuclear",
    "HYD" = "Hydro",
    "WND" = "Wind",
    "SOL" = "Solar",
    "GEO" = "Geothermal"
  )
  
  df_rename_EF <- data.frame(
    Code = names(rename_EF),
    Fuel = as.vector(rename_EF),
    stringsAsFactors = FALSE
  )
  
  historical <- getEmberCap() %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value"))
  
  historical <- as.quitte(historical) %>% as.magpie()
  
  PGALLtoEF <- toolGetMapping(name = "PGALLtoEF.csv",
                              type = "blabla_export",
                              where = "mrprom")
  
  PGALLtoEF <- PGALLtoEF[PGALLtoEF[,"PGALL"] %in% intersect(getItems(historical,3),PGALLtoEF[,1]),]
  
  historical <- toolAggregate(historical, dim = 3, rel = PGALLtoEF, from = "PGALL", to = "EF")
  historical <- toolAggregate(historical, dim = 3, rel = df_rename_EF, from = "Code", to = "Fuel")
  
  getItems(historical, 3) <- paste0("Capacity|Electricity","|", getItems(historical, 3))
  
  years_in_horizon <-  horizon[horizon %in% getYears(historical, as.integer = TRUE)]
  
  # write data in mif file
  write.report(historical[, years_in_horizon, ],file = "reporting.mif", model = "EmberCapacity", unit = "GW", append = TRUE, scenario = "fullVALIDATION2")
  
  ################
  
  ######### Ember Capacity
  
  rename_EF <- c(
    "LGN" = "Lignite",
    "HCL" = "Hard coal",
    "GDO" = "Oil",
    "NGS" = "Gas",
    "BMSWAS" = "Biomass",
    "NUC" = "Nuclear",
    "HYD" = "Hydro",
    "WND" = "Wind",
    "SOL" = "Solar",
    "GEO" = "Geothermal"
  )
  
  df_rename_EF <- data.frame(
    Code = names(rename_EF),
    Fuel = as.vector(rename_EF),
    stringsAsFactors = FALSE
  )
  
  historical <- getEmberProdElec() %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value"))
  
  historical <- as.quitte(historical) %>% as.magpie()
  
  PGALLtoEF <- toolGetMapping(name = "PGALLtoEF.csv",
                              type = "blabla_export",
                              where = "mrprom")
  
  PGALLtoEF <- PGALLtoEF[PGALLtoEF[,"PGALL"] %in% intersect(getItems(historical,3),PGALLtoEF[,1]),]
  
  historical <- toolAggregate(historical, dim = 3, rel = PGALLtoEF, from = "PGALL", to = "EF")
  historical <- toolAggregate(historical, dim = 3, rel = df_rename_EF, from = "Code", to = "Fuel")
  
  getItems(historical, 3) <- paste0("Secondary Energy|Electricity","|", getItems(historical, 3))
  
  years_in_horizon <-  horizon[horizon %in% getYears(historical, as.integer = TRUE)]
  
  # write data in mif file
  write.report(historical[, years_in_horizon, ],file = "reporting.mif", model = "EmberSE", unit = "TWh", append = TRUE, scenario = "fullVALIDATION2")
  
  ################
  
  # rename mif file
  fullVALIDATION2 <- read.report("reporting.mif")
  write.report(fullVALIDATION2, file = paste0("fullVALIDATION2.mif"))
  
  return(list(x = NULL,
              weight = NULL,
              unit = NULL,
              description = "VALIDATION2"))
  
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
  data[,2021,] <- data[,2020,]
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

