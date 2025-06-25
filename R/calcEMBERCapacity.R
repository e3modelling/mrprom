#' calcEMBERCapacity
#'
#' Use data from EMBER Capacity, in GW.
#'
#' @return magpie object
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "EMBERCapacity", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select filter rename mutate case_when
#' @importFrom tidyr pivot_wider spread gather
#' @importFrom quitte as.quitte interpolate_missing_periods

calcEMBERCapacity <- function() {
  
  capacities <- readSource("EMBER", convert = TRUE)
  capacities <- capacities[,,"Capacity"]
  capacities <- collapseDim(capacities,3.3)
  
  mapEMBER <- data.frame(
    EMBER = c("Bioenergy","Coal","Gas","Hydro","Nuclear","Other Fossil",
    "Other Renewables","Solar","Wind"),
    OPEN_PROM = c("ATHBMSWAS","ATHCOAL","ATHGAS","PGLHYD","PGANUC","ATHOIL",
                  "PGOTHREN","PGSOL","PGAWND")
  )
  
  # aggregate from ENERDATA fuels to reporting fuel categories
  capacities <- toolAggregate(capacities,dim = 3.1,rel = mapEMBER,from = "EMBER",to = "OPEN_PROM")
  
  ATHLGN <- capacities[,,"ATHCOAL"]
  getItems(ATHLGN, 3.1) <- "ATHLGN"
  PGCSP <- capacities[,,"PGSOL"]
  getItems(PGCSP, 3.1) <- "PGCSP"
  PGSHYD <- capacities[,,"PGLHYD"]
  getItems(PGSHYD, 3.1) <- "PGSHYD"
  PGAWNO <- capacities[,,"PGAWND"]
  getItems(PGAWNO, 3.1) <- "PGAWNO"
  
  capacities <- mbind(capacities, ATHLGN, PGCSP, PGSHYD, PGAWNO)
  
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
    interpolate_missing_periods(period = seq(2010, 2024, 1), expand.values = TRUE) %>%
    select(c("region", "period", "variable", "value"))
  
  techProd_data <- as.quitte(capacities)
  techProd_data <- select(techProd_data,c("region","variable","period","value"))
  
  shares <- Reduce(
    function(x, y) full_join(x, y, by = c("region", "period", "variable")),
    list(
      getSharesTech(take_shares, techProd_data, c("PGSOL", "PGCSP")),
      getSharesTech(take_shares, techProd_data, c("PGLHYD", "PGSHYD")),
      getSharesTech(take_shares, techProd_data, c("PGAWND", "PGAWNO")),
      getSharesTech(take_shares, techProd_data, c("ATHCOAL", "ATHLGN"))
    )
  ) %>%
    mutate(value = coalesce(value.x,value.y,value.x.x,value.y.y)) %>%
    select(region, period, variable, value)
  
  techProd <- techProd_data %>%
    left_join(shares, by = c("region", "variable", "period")) %>%
    mutate(value = ifelse(is.na(value.y), value.x, value.y)) %>%
    select(c("region", "period", "variable", "value"))
  
  techProd <- as.quitte(techProd) %>% as.magpie()
  
  # Set NA to 0
  techProd[is.na(techProd)] <- 0
  
  list(x = techProd,
       weight = NULL,
       unit = "GW",
       description = "EMBERCapacity")
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
