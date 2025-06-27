#' calcPrimesPG
#'
#' Use Primes data Secondary Energy Electricity, Gross Electricity generation by plant type
#' and Net Installed Power Capacity
#' 
#' @param subtype "SE" for SE Electricity or "power generation" for
#' Gross Electricity generation by plant type and "capacity" for
#' Net Installed Power Capacity
#' 
#' @return Primes PG data
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "PrimesPG",subtype = "SE", aggregate = FALSE)
#' }
#'
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom utils tail
#' @importFrom dplyr %>% select filter rename mutate case_when group_by ungroup


calcPrimesPG <- function(subtype = "SE") {
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  
  if (subtype  == "SE") {
    #Primes data
    a <- readSource("PrimesPGData", subtype = subtype)
    
    a <- a[getRegions(a)[getRegions(a) %in% as.character(getISOlist())], , ]
    a <- toolCountryFill(a, fill = NA)
  }
  
  if (subtype  == "capacity") {
    
    #Primes data
    a <- readSource("PrimesPGData", subtype = subtype)
    
    mapping <- list(
      primes = c(
        "Nuclear energy","Lakes","Run of river", "Wind on-shore",
        "Wind off-shore", "Solar","Solids fired", "Oil fired", "Gas fired",
        "Biomass-waste fired","Geothermal heat"),
      openprom = c(
        "PGANUC","PGLHYD","PGSHYD", "PGAWND",
        "PGAWNO", "PGSOL","ATHCOAL", "ATHOIL", "ATHGAS",
        "ATHBMSWAS", "PGOTHREN"))
    
    mapping <- as.data.frame(mapping)
    
    capacities <- toolAggregate(a[, , as.character(unique(mapping[["primes"]]))], dim = 3.2, rel = mapping, from = "primes", to = "openprom")
  
    capacities <- collapseDim(capacities,3.1)
    
    ATHLGN <- capacities[,,"ATHCOAL"]
    getItems(ATHLGN, 3.1) <- "ATHLGN"
    PGCSP <- capacities[,,"PGSOL"]
    getItems(PGCSP, 3.1) <- "PGCSP"
    
    capacities <- mbind(capacities, ATHLGN, PGCSP)
    
    capacities <- capacities[getRegions(capacities)[getRegions(capacities) %in% as.character(getISOlist())], , ]
    capacities <- toolCountryFill(capacities, fill = NA)
    
    capacities <- as.quitte(capacities) %>% as.magpie()
    
    data <- readSource("ENERDATA", "capacity", convert = TRUE)
    data[is.na(data)] <- 0
    data[,,"Total electricity capacity coal, lignite (multifuel included)"] <- data[,,"Total electricity capacity coal, lignite (multifuel included)"] - data[,,"Single fired electricity capacity lignite"]
    
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
    ) %>% select(c("PROM_Code","ENERDATA_Name")) %>% 
      filter(PROM_Code %in% sets) %>%
      separate_rows(PROM_Code, sep = ",") %>%
      rename(product = ENERDATA_Name, variable = PROM_Code) %>% na.omit(map)
    
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
      interpolate_missing_periods(period = min(getYears(capacities,as.integer=TRUE)):max(getYears(capacities,as.integer=TRUE)), expand.values = TRUE) %>%
      select(c("region", "period", "variable", "value"))
    
    techProd_data <- as.quitte(capacities)
    techProd_data <- select(techProd_data,c("region","variable","period","value"))
    
    shares <- Reduce(
      function(x, y) full_join(x, y, by = c("region", "period", "variable")),
      list(
        getSharesTech(take_shares, techProd_data, c("PGSOL", "PGCSP")),
        getSharesTech(take_shares, techProd_data, c("ATHCOAL", "ATHLGN"))
      )
    ) %>%
      mutate(value = coalesce(value.x,value.y)) %>%
      select(region, period, variable, value)
    
    techProd <- techProd_data %>%
      left_join(shares, by = c("region", "variable", "period")) %>%
      mutate(value = ifelse(is.na(value.y), value.x, value.y)) %>%
      select(c("region", "period", "variable", "value"))
    
    a <- as.quitte(techProd) %>% as.magpie()
    
    a <- add_dimension(a, dim = 3.2, add = "unit", nm = "GW")
    
  }
  
  if (subtype  == "power generation") {
    
    share_of_solar <- calcOutput(type = "IInstCapPast", aggregate = FALSE)
    
    #Primes data
    a <- readSource("PrimesPGData", subtype = subtype)
    
    mapping <- list(
      primes = c(
        "Nuclear energy","Lakes","Run of river", "Wind on-shore",
        "Wind off-shore", "Solar","Solids fired", "Oil fired", "Gas fired",
        "Biomass-waste fired","Geothermal heat"),
      openprom = c(
        "PGANUC","PGLHYD","PGSHYD", "PGAWND",
        "PGAWNO", "PGSOL","ATHCOAL", "ATHOIL", "ATHGAS",
        "ATHBMSWAS", "PGOTHREN"))
    
    mapping <- as.data.frame(mapping)
    
    generation <- toolAggregate(a[, , as.character(unique(mapping[["primes"]]))], dim = 3.2, rel = mapping, from = "primes", to = "openprom")
    
    generation <- collapseDim(generation,3.1)
    
    ATHLGN <- generation[,,"ATHCOAL"]
    getItems(ATHLGN, 3.1) <- "ATHLGN"
    PGCSP <- generation[,,"PGSOL"]
    getItems(PGCSP, 3.1) <- "PGCSP"
    
    generation <- mbind(generation, ATHLGN, PGCSP)
    
    generation <- generation[getRegions(generation)[getRegions(generation) %in% as.character(getISOlist())], , ]
    generation <- toolCountryFill(generation, fill = NA)
    
    generation <- as.quitte(generation) %>% as.magpie()
    
    data <- readSource("ENERDATA", "production", convert = TRUE)
    data[is.na(data)] <- 0
    data[,,"Electricity production from coal, lignite.GWh"] <- data[,,"Electricity production from coal, lignite.GWh"] - data[,,"Electricity production from coal"]
    
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
      name = "prom-enerdata-elecprod-mapping.csv",
      type = "sectoral",
      where = "mrprom"
    ) %>% select(c("PGALL","ENERDATA")) %>% 
      filter(PGALL %in% sets) %>%
      separate_rows(PGALL, sep = ",") %>%
      rename(product = ENERDATA, variable = PGALL) %>% na.omit(map)
    
    names(data) <- sub("variable", "product", names(data))
    
    data <- filter(data, unit == "GWh")
    
    map[["product"]] <- sub("\\..*", "", map[["product"]])
    
    # group by each technology and sum over its sub-technologies
    techProd <- data %>%
      left_join(map, by = "product", relationship = "many-to-many") %>%
      select(c("region", "period", "value", "variable")) %>%
      group_by(region, period, variable) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      drop_na()
    
    techProd <- as.quitte(techProd)
    techProd <- as.magpie(techProd)
    
    techProd <- add_dimension(techProd, dim = 3.2, add = "unit", nm = "TWh")
    techProd <- techProd / 1000
    
    #share of PV, CSP
    share_of_PV <- share_of_solar[,,"PGSOL"] / (share_of_solar[,,"PGCSP"] + share_of_solar[,,"PGSOL"])
    share_of_CSP <- share_of_solar[,,"PGCSP"] / (share_of_solar[,,"PGCSP"] + share_of_solar[,,"PGSOL"])
    techProd[,,"PGCSP"] <- techProd[,,"PGCSP"] * ifelse(is.na(share_of_CSP), mean(share_of_CSP,na.rm = TRUE), share_of_CSP)
    techProd[,,"PGSOL"] <- techProd[,,"PGSOL"] * ifelse(is.na(share_of_PV), mean(share_of_PV,na.rm = TRUE), share_of_PV)
    
    take_shares <- techProd
    
    take_shares <- as.quitte(take_shares) %>%
      interpolate_missing_periods(period = min(getYears(generation,as.integer=TRUE)):max(getYears(generation,as.integer=TRUE)), expand.values = TRUE) %>%
      select(c("region", "period", "variable", "value"))
    
    techProd_data <- as.quitte(generation)
    techProd_data <- select(techProd_data,c("region","variable","period","value"))
    
    shares <- Reduce(
      function(x, y) full_join(x, y, by = c("region", "period", "variable")),
      list(
        getSharesTech(take_shares, techProd_data, c("PGSOL", "PGCSP")),
        getSharesTech(take_shares, techProd_data, c("ATHCOAL", "ATHLGN"))
      )
    ) %>%
      mutate(value = coalesce(value.x,value.y)) %>%
      select(region, period, variable, value)
    
    techProd <- techProd_data %>%
      left_join(shares, by = c("region", "variable", "period")) %>%
      mutate(value = ifelse(is.na(value.y), value.x, value.y)) %>%
      select(c("region", "period", "variable", "value"))
    
    a <- as.quitte(techProd) %>% as.magpie()
    
    a <- add_dimension(a, dim = 3.2, add = "unit", nm = "TWh")
    
  }
  
  a <- as.quitte(a) %>%
    interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
  
  a <- as.quitte(a) %>% as.magpie()
  
  # set NA to 0
  a[is.na(a)] <- 10^-6
  a <- a[,fStartHorizon : 2100,]
  
  list(x = a,
       weight = NULL,
       unit = "various",
       description = "Primes Secondary Energy Electricity Gross Electricity generation by plant type
      and Net Installed Power Capacity")
  
}

# Helper ------------------------------------------------
getSharesTech <- function(take_shares, techProd_data, vars) {
  shares <- take_shares %>%
    as.quitte() %>%  select(c("region","variable","period","value"))%>% 
    filter(variable %in% vars) %>%
    group_by(region, period) %>% # Group by region and period
    mutate(total_value = sum(value), share = value / total_value) %>%
    select(c("region", "variable", "period", "share")) %>%
    replace_na(list(share = 0)) %>%
    right_join(techProd_data, by = c("region", "period", "variable")) %>%
    mutate(value = value * share) %>%
    select(-share) %>% ungroup()
  return(shares)
}
