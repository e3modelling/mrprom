#' calcINDISFuelConsumptionIEA
#'
#' Calculates fuel consumption in the Iron and Steel sector, corresponding to the 
#' variable "iFuelConsINDSE" for the "IS" sector in Open PROM. The calculation is 
#' performed for key regions according to two IEA scenarios: Stated Policies (IEA STEPS) 
#' and Sustainable Development (IEA SDS).
#'
#' The function provides:
#' - A common value for fuel consumption in 2019 across both scenarios.
#' - Separate values for 2050 under IEA STEPS and IEA SDS.
#'
#' The calculation is performed for each fuel type by determining the consumption 
#' for each technological route (e.g., BF-BOF, DR-EAF, etc.), using:
#' 1. Technological shares (%) per region from the IEA Iron and Steel Roadmap.
#' 2. Steel production from WEO 2023 Extended Data in Mton of steel per year
#' 3. Specific fuel consumption per technological route from IEA The Future of Hydrogen in PJ/Mton of steel
#' The results of fuel consumption for fuel type in PJ/year is converted in Mtoe/year
#' 
#' Current Limitations:
#' - The function currently includes coal, natural gas, and electricity consumption, 
#'   as fuel-specific consumption values are globally defined without regional differentiation.
#' - Since the final consumption of fuel is based on generic fuel specific consumption per technological 
#'   route, the consumption for 2019 can be slightly underestimate, especially in country with old BF-BOF plants
#' - Future improvements could extend the calculation to oil, biomass, and heat using 
#'   country-level fuel shares from the IEA Iron and Steel Roadmap. It can be introduced country-specific parameters
#'   for calibrating the consumption of 2019 according to the energy balances
#' - Coal consumption includes usage in blast furnaces (BF), which is currently 
#'   classified under transformations in Open PROM.
#' - Further regional disaggregation may be necessary for MEA, Africa, and Central 
#'   and South America. However, for Europe, PRIMES data is available, and additional 
#'   disaggregation may introduce significant approximations.
#'
#' @return magpie object with OPENPROM input data iDataConsEneBranch.
#'
#' @author Sonja Sechi, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "INDISFuelConsumptionIEA", aggregate = FALSE)
#' }
#' @importFrom dplyr %>% select mutate left_join case_when if_else arrange
#' @importFrom quitte as.quitte
#' @importFrom tidyr drop_na nesting expand complete
#' 
  calcINDISFuelConsumptionIEA <- function(convfact = 1) {
  

  # Read and convert IEA Industry Roadmaps data
  industry_data <- readSource("IEA_Industry_Roadmaps", convert = FALSE)
  x <- as.quitte(industry_data)
  x <- filter(x, !is.na(x[["value"]]))
  x <- filter(x, !is.na(x[["region"]]))
  # Manually map of the regions, converting only China, India, and USA to ISO codes
  # Assign custom codes for aggregated regions
  x[["region"]] <- toolCountry2isocode(x[["region"]], mapping = c(
    "China" = "CHN",
    "India" = "IND",
    "United States" = "USA",
    "European Union" = "EUR",          
    "Middle East" = "MEA",             
    "Central and South America" = "LAM",
    "Africa" = "SSA"
  ))
  
  x <- filter(x, !is.na(x[["value"]]))
  x <- filter(x, !is.na(x[["region"]]))
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  regs <- !(getISOlist() %in% getRegions(x))
  
  x <- add_columns(x, addnm = as.character(getISOlist()[regs]), dim = 1, fill = NA)
  
  industry_data <-as.quitte(x)
  #convert with ISO codes when possible, otherwise keep the original IEA aggregate region

  tech_data <- readSource("IEA_Industry_Roadmaps", subtype = "IEA_Tech_Assumptions", convert = FALSE)
  tech_data <-as.quitte(tech_data)
  # Read and convert WEO 2023 Extended Data

  # Read and convert WEO 2023 Extended Data
  weo_data <- readSource("IEA_WEO_2023_ExtendedData", convert = FALSE)
  y <- as.quitte(weo_data)
  
  y <- filter(y, !is.na(y[["value"]]))
  y <- filter(y, !is.na(y[["region"]]))
  
  #Manually map the regions to ISO codes where applicable
  y[["region"]] <- toolCountry2isocode(y[["region"]], mapping = c(
    "China" = "CHN",
    "India" = "IND",
    "United States" = "USA",
    "European Union" = "EUR",          
    "Middle East" = "MEA",             
    "Central and South America" = "LAM",
    "Africa" = "SSA"
  ))
  
  y <- filter(y, !is.na(y[["value"]]))
  y <- filter(y, !is.na(y[["region"]]))
  
  y <- as.quitte(y)
  y <- as.magpie(y)
  
  regsy <- !(getISOlist() %in% getRegions(y))
  
  y <- add_columns(y, addnm = as.character(getISOlist()[regsy]), dim = 1, fill = NA)
  
 weo_data <-as.quitte(y)
 
 weo_data <- filter(weo_data, !is.na(weo_data[["value"]]))
 weo_data <- filter(weo_data, !is.na(weo_data[["region"]]))
 
  # keep once regions from industry_data
  regions_industrydata <- industry_data %>% distinct(region) 
  # Filters WEO to keep ony regions available in industry_data
  weo_filtered <- weo_data %>% filter(region %in% regions_industrydata[["region"]])
  

  #y <- drop_na(y) to eliminate na value
  # Convert again to a quitte object
  #weo <- as.quitte(y)

  

  # Assign missing fuel intensity values for specific technologies that has not be included in the
  #IEA technological Assumptions
  tech_data[which(tech_data[, 8] == "Commercial SR-BOF"&tech_data[, 9] == "coal"), 7] <- 0
  tech_data[which(tech_data[, 8] == "Commercial SR-BOF"&tech_data[, 9] == "coal with CCUS"), 7] <- 12
  tech_data[which(tech_data[, 8] == "Commercial SR-BOF"&tech_data[, 9] == "oil"), 7] <- 0
  tech_data[which(tech_data[, 8] == "Commercial SR-BOF"&tech_data[, 9] == "gas"), 7] <- 1
  tech_data[which(tech_data[, 8] == "Commercial SR-BOF"&tech_data[, 9] == "gas with CCUS"), 7] <- 0
  tech_data[which(tech_data[, 8] == "Commercial SR-BOF"&tech_data[, 9] == "bioenergy"), 7] <- 0
  tech_data[which(tech_data[, 8] == "Commercial SR-BOF"&tech_data[, 9] == "electricity"), 7] <- 2
  tech_data[which(tech_data[, 8] == "Commercial SR-BOF"&tech_data[, 9] == "electricity for H2"), 7] <- 0
  tech_data[which(tech_data[, 8] == "Commercial SR-BOF"&tech_data[, 9] == "imported heat"), 7] <- 0
  
  tech_data[which(tech_data[, 8] == "Innovative BF-BOF with CCUS"&tech_data[, 9] == "coal"), 7] <- 0
  tech_data[which(tech_data[, 8] == "Innovative BF-BOF with CCUS"&tech_data[, 9] == "coal with CCUS"), 7] <- 18
  tech_data[which(tech_data[, 8] == "Innovative BF-BOF with CCUS"&tech_data[, 9] == "oil"), 7] <- 0
  tech_data[which(tech_data[, 8] == "Innovative BF-BOF with CCUS"&tech_data[, 9] == "gas"), 7] <- 1
  tech_data[which(tech_data[, 8] == "Innovative BF-BOF with CCUS"&tech_data[, 9] == "gas with CCUS"), 7] <- 0
  tech_data[which(tech_data[, 8] == "Innovative BF-BOF with CCUS"&tech_data[, 9] == "bioenergy"), 7] <- 0
  tech_data[which(tech_data[, 8] == "Innovative BF-BOF with CCUS"&tech_data[, 9] == "electricity"), 7] <- 3.5
  tech_data[which(tech_data[, 8] == "Innovative BF-BOF with CCUS"&tech_data[, 9] == "electricity for H2"), 7] <- 0
  tech_data[which(tech_data[, 8] == "Innovative BF-BOF with CCUS"&tech_data[, 9] == "imported heat"), 7] <- 0


  # Extract Iron and Steel production for 2021 and use it as a proxy for 2019
  weo_filtered <- weo_data %>%
    filter(variable == "Iron and steel", period %in% c(2021, 2050),
           scenario %in% c("Stated Policies Scenario", "Announced Pledges Scenario")) %>%
    select(scenario, region, period, value) %>%
    rename(steel_production = value, ytime = period)
  
  # Ensure 2019 values are shared across both scenarios (using 2021 as a proxy for 2019)
  common_2019 <- weo_filtered %>% filter(ytime == 2021) %>% mutate(ytime = 2019)
  weo_filtered <- bind_rows(common_2019, weo_filtered)
  

  # Keep only regions that exist in the IEA Industry Roadmaps dataset
  weo_filtered <- weo_filtered %>%
    filter(region %in% unique(industry_data[["region"]]))
  weo_filtered <- drop_na(weo_filtered)
  
  #data framweork with the steel production expressed in Million tons 
  #for 2019 historic data, 2050 IEA STEPS and IEA SDS
  steel_production_weo <- weo_filtered %>%
    mutate(
      ytime = ifelse(ytime == 2021, 2019, ytime),
      scenario = ifelse(ytime == 2019 & scenario == "Stated Policies Scenario", "historic IEA",
                        ifelse(ytime == 2050 & scenario == "Stated Policies Scenario", "IEA STEPS",
                               ifelse(ytime == 2050 & scenario == "Announced Pledges Scenario", "IEA SDS", scenario))))
 
   # keep once technologies route from tech_data
  techs_tech_data <- tech_data %>% distinct(technology) 

  # filter technological share in industry_data 

 industry_data <- industry_data %>%
    filter(variable %in% techs_tech_data[["technology"]]) #the name of variable in the vector tech_tech_data

    
    names(steel_production_weo)<-sub("ytime","period",names(steel_production_weo))
    names(steel_production_weo)<-sub("steel_production","value",names(steel_production_weo))
    
    calc_matrix <-left_join(industry_data, steel_production_weo, by = c("scenario", "region", "period")) 
    calc_matrix <- distinct(calc_matrix)
    names(calc_matrix)<-sub("value.x","value",names(calc_matrix))
    names(calc_matrix)<-sub("value.y","steel_production",names(calc_matrix))
    names(tech_data)<-sub("fuel.type","fuel",names(tech_data))
    tech_data <- select(tech_data,-("variable" ))
    names(tech_data)<-sub("technology","variable",names(tech_data))
    tech_data <- select(tech_data,c("value","variable", "fuel" ))
    
    tech_data <- expand(tech_data, nesting(variable, fuel, value), scenario = as.character(unique(calc_matrix[["scenario"]])))
    tech_data <- expand(tech_data, nesting(variable, fuel, value, scenario), period = c(2019,2050))
    
    calc_matrix <-left_join(calc_matrix, tech_data, by = c("period", "variable","scenario")) 
    calc_matrix <- distinct(calc_matrix)
    calc_matrix["IS_fuel_consumption"] <- calc_matrix["value.x"] /100* calc_matrix["value.y"] *calc_matrix["steel_production"]
    calc_matrix <- mutate(calc_matrix, IS_fuel_aggregated = sum(IS_fuel_consumption), .by = c("region", "period","fuel","scenario"))
    calc_aggregated <- select(calc_matrix,c("scenario","region", "period" ,"fuel","IS_fuel_aggregated" )) 
    calc_aggregated <- distinct(calc_aggregated)
    
    names(calc_aggregated)<-sub("IS_fuel_aggregated","value",names(calc_aggregated))
  
  
    calc_aggregated <- as.quitte(calc_aggregated) %>% as.magpie()
 

  # Fuel mapping to OPEN-PROM naming conventions
  fuel_map <- data.frame(
    fuel = c("coal", "coal with CCUS", "gas", "gas with CCUS", "bioenergy", 
             "electricity", "electricity for H2", "oil", "imported heat"),
    EF = c("HCL", "HCL", "NGS", "NGS", "BMSWAS", "ELC", "ELC", "CRO", "STE1AM")
)
 
  IS_fuel_prom <- toolAggregate(calc_aggregated[,,fuel_map[,"fuel"]], dim = 3.2,rel = fuel_map,from = "fuel", to = "EF")
  IS_fuel_prom <- IS_fuel_prom / 41.868
  
  qx <- as.quitte(IS_fuel_prom)
  
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  
  qx_bu <- qx
  
  GDP <- calcOutput(type = "iGDP", aggregate = FALSE)
  GDP <- as.quitte(GDP)
  
  # compute weights by GDP
  names(GDP) <- sub("region", "CountryCode", names(GDP))
  
  ## add mapping to GDP
  GDP <- left_join(GDP, h12, by = "CountryCode")
  GDP2 <- GDP
  value.x <- NULL
  value.y <- NULL
  weights <- NULL
  value <- NULL
  GDP <- mutate(GDP, weights = sum(value, na.rm = TRUE), .by = c("RegionCode", "period"))
  GDP["weights"] <- GDP["value"] / GDP["weights"]
  
  names(GDP) <- sub("CountryCode", "region", names(GDP))
  GDP <- select(GDP, -c("value", "model", "scenario", "X", "variable", "unit"))
  qx <- left_join(qx, GDP, by = c("region", "period"))
  
  qx <- qx %>% mutate(RegionCode = ifelse(is.na(RegionCode), region, RegionCode)) %>%
    mutate(weights = ifelse(is.na(weights), 1, weights))
  
  qx <- mutate(qx, value = sum(value, na.rm = TRUE), .by = c("scenario","RegionCode", "period", "fuel", "variable", "unit"))
  
  qx["value"] <- qx["value"] * qx["weights"]
  
  qx <- select(qx, -c("weights"))
  
  qx <- left_join(qx_bu, qx, by = c("scenario","region", "variable", "period", "fuel", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y", "RegionCode", "model.y"))
  
  names(qx)<-sub("model.x","model",names(qx))
  
  qx <- filter(qx, !is.na(qx[["value"]]))
  
  qx <- as.quitte(qx)
  x <- as.magpie(qx)
    
  x <- add_dimension(x, dim = 3.3, add = "unit", nm = "Mtoe")
  
  x <- x[as.character(getISOlist()), , ]
  
  list(x = x,
       weight = NULL,
       unit = "Mtoe",
       description = "IEA INDUSTRY; Fuel Consumption")
  }




