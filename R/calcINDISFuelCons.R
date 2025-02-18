calcINDISFuelConsumption_IEA <- function(convfact = 1) {
  
  # Read and convert IEA Industry Roadmaps data
 
  industry_data <- readSource("IEA_Industry_Roadmaps", convert = FALSE)
  #covert the magpie object in a dataframe
  industry_data <-as.quitte(industry_data)
 # Extract regions (from the error it seems that region was stored not correctly
industry_data$region <- getRegions(industry_data)
 
  #convert with ISO codes when possible, otherwise keep the original IEA aggregate region
  industry_data <- convertIEA_Industry_Roadmaps(industry_data)
  
  tech_data <- readSource("IEA_Industry_Roadmaps", subtype = "IEA_Tech_Assumptions", convert = FALSE)$x
  
  # Read and convert WEO 2023 Extended Data

 
  weo_data <- readSource("IEA_WEO_2023_ExtendedData", subtype = "IEA_WEO_2023_ExtendedData", convert = FALSE)$x
  weo_data <-as.quitte(weo_data)
  weo_data <- convertIEA_WEO_2023_ExtendedData(weo_data)  

  # Ensure consistent column names
  colnames(industry_data) <- tolower(colnames(industry_data))
  colnames(tech_data) <- tolower(colnames(tech_data))
  colnames(weo_data) <- tolower(colnames(weo_data))

  # Assign missing fuel intensity values for specific technologies
  tech_data <- tech_data %>%
    mutate(fuel_intensity = case_when(
      variable == "Commercial SR-BOF" & fuel == "coal with CCS" ~ 18,
      variable == "Innovative BF-BOF with CCUS" & fuel == "coal with CCS" ~ 12,
      variable == "Commercial SR-BOF" & fuel == "electricity" ~ 2,
      variable == "Innovative BF-BOF with CCUS" & fuel == "electricity" ~ 3.5,
      variable == "Commercial SR-BOF" & fuel == "gas" ~ 1,
      variable == "Innovative BF-BOF with CCUS" & fuel == "gas" ~ 0,
      TRUE ~ fuel_intensity  # Keep existing values if no match
    ))

  # Map IEA scenarios to WEO scenarios
  # Note: Since WEO does not have steel production for IEA SDS, 
  # we use the "Announced Pledges Scenario" as a reference for IEA SDS.
  industry_data <- industry_data %>%
    mutate(scenario = case_when(
      scenario == "IEA STEPS" ~ "Stated Policies Scenario",
      scenario == "IEA SDS" ~ "Announced Pledges Scenario", # Using Announced Pledges Scenario as SDS reference
      scenario == "historic IEA" ~ "Stated Policies Scenario",
      TRUE ~ scenario
    ))

  # Extract Iron and Steel production for 2021 and use it as a proxy for 2019
  weo_filtered <- weo_data %>%
    filter(variable == "Iron and Steel", period %in% c(2021, 2050),
           scenario %in% c("Stated Policies Scenario", "Announced Pledges Scenario")) %>%
    select(scenario, region, period, value) %>%
    rename(steel_production = value, ytime = period)
  
  # Ensure 2019 values are shared across both scenarios (using 2021 as a proxy for 2019)
  common_2019 <- weo_filtered %>% filter(ytime == 2021) %>% mutate(ytime = 2019)
  weo_filtered <- bind_rows(common_2019, weo_filtered)

  # Rename "Announced Pledges Scenario" to "IEA SDS" in WEO data to match industry_data
  weo_filtered <- weo_filtered %>%
    mutate(scenario = ifelse(scenario == "Announced Pledges Scenario", "IEA SDS", scenario))

  # Keep only regions that exist in the IEA Industry Roadmaps dataset
  weo_filtered <- weo_filtered %>%
    filter(region %in% unique(industry_data$region))

  # Compute fuel consumption by iterating over each fuel type
  fuel_types <- unique(tech_data$fuel)
  fuel_consumption_results <- list()

  for (fuel in fuel_types) {
    fuel_subset <- tech_data %>% filter(fuel == !!fuel)
    
    fuel_calculation <- fuel_subset %>%
      left_join(industry_data, by = c("scenario", "region", "ytime")) %>%
      left_join(weo_filtered, by = c("scenario", "region", "ytime")) %>%
      mutate(
        fuel_consumption = steel_production * (tech_share / 100) * fuel_intensity * convfact
      ) %>%  # Removed fuel_share from the calculation
      select(region, variable, fuel, ytime, fuel_consumption)
    
    fuel_consumption_results[[fuel]] <- fuel_calculation
  }

  final_output <- bind_rows(fuel_consumption_results)

  # Fuel mapping to OPEN-PROM naming conventions
  fuel_map <- data.frame(
    fuel = c("coal", "coal with CCS", "gas", "gas with CCUS", "bioenergy", 
             "electricity", "electricity for H2", "oil", "imported heat"),
    EF = c("HCL", "HCL", "NGS", "NGS", "BMSWAS", "ELC", "ELC", "CRO", "STE1AM")
  )

  final_output <- final_output %>%
    left_join(fuel_map, by = "fuel") %>%
    select(region, variable, EF, ytime, fuel_consumption)

  # Rename columns for final structure
  final_output <- final_output %>%
    rename(allCy = region, INDSE = variable, EF = EF, Value = fuel_consumption)

  # Create aggregated fuel categories
  aggregated_output <- final_output %>%
    group_by(allCy, INDSE, EF, ytime) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

  # Convert to quitte and magpie objects for final Open-PROM format
  final_output_quitte <- as.quitte(final_output)
  final_output_magpie <- as.magpie(final_output)

  aggregated_output_quitte <- as.quitte(aggregated_output)
  aggregated_output_magpie <- as.magpie(aggregated_output)

  # Save results in Excel for verification - to be removed later
  write.csv(final_output, "Fuel_Consumption_Detailed.csv", row.names = FALSE)
  write.csv(aggregated_output, "Fuel_Consumption_Aggregated.csv", row.names = FALSE)

  # Return final processed datasets
  list(
    detailed_quitte = final_output_quitte,
    detailed_magpie = final_output_magpie,
    aggregated_quitte = aggregated_output_quitte,
    aggregated_magpie = aggregated_output_magpie
  )
}
