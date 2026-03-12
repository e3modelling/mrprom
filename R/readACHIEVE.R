#' readACHIEVE
#'
#' Read in a XLSX file from ACHIEVE and convert it to a magpie object.
#' 
#'
#' @return magpie object with the requested output data
#'
#' @author Fotis Sioutas, Dionisis
#'
#' @examples
#' \dontrun{
#' a <- readSource("ACHIEVE")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>%  select left_join mutate rowwise ungroup
#' @importFrom readxl read_excel excel_sheets
#' @importFrom tidyr pivot_longer
#' @importFrom purrr flatten map map_dfr
#'

readACHIEVE <- function() {
  
  # List all .xlsx files in the folder
  files <- list.files(pattern = "\\.xlsx$", full.names = TRUE)
  files <- c(
    "./GrowthRateVariables.xlsx",
    "./r2z_Mapping.xlsx",
    "./r2z_regions_ambition_pathways_2024.xlsx"
  )
  
  # Read all sheets from all files into a flat list
  data_list <- map(files, function(file) {
    
    # Get sheet names in the current Excel file
    sheets <- excel_sheets(file)
    
    # Read all sheets and name them file_sheet
    map(sheets, function(sheet) {
      read_excel(file, sheet = sheet)
    }) |> 
      set_names(paste0(tools::file_path_sans_ext(basename(file)), "_", sheets))
    
  }) |> 
    flatten()  # flatten to a single list
  
  for (i in seq_along(data_list)) {
    assign(names(data_list)[i], data_list[[i]])
  }
  
  Target_CP <- r2z_regions_ambition_pathways_2024_Target_CP %>%
    pivot_longer(
      cols = `2023`:`2099`,
      names_to = "period",
      values_to = "value"
    )  %>% rename(CDP_ISO = country_of_emissions) %>%
    rename(`R2Z sector` = sector)
  
  r2z_CP <- left_join(Target_CP, r2z_Mapping_Regional, by = "CDP_ISO")
  r2z_CP <- left_join(r2z_CP, r2z_Mapping_Sectoral, by = "R2Z sector")
  
  # List of target year columns
  target_cols <- paste0("target_year_", 1:5)
  
  r2z_CP <- r2z_CP %>%
    mutate(
      period = as.numeric(period),
      across(all_of(target_cols), as.numeric)
    ) %>%
    rowwise() %>%  # process row by row
    mutate(
      max_target = max(c_across(all_of(target_cols)), na.rm = TRUE),  # max target ignoring NA
      emissions = if(!is.na(max_target) & period <= max_target) value else NA_real_
    ) %>%
    ungroup() %>%
    select(-max_target) 
  
  growth_OP <- GrowthRateVariables_GrowthRateVariables[,c("region", "variable", "period",  "value")]
  names(growth_OP) <- c("OPEN_PROM", "OPEN-PROM Subsector Emission Growth Rates", "period", "emissions")
  
  if(!2100 %in% r2z_CP$period) {
    
    # Get the last year (2099) row(s)
    last_year_row <- r2z_CP %>% filter(period == max(period))
    
    # Copy and set period to 2100
    new_row <- last_year_row %>% mutate(period = 2100)
    
    # Bind the new row to the dataset
    r2z_CP <- bind_rows(r2z_CP, new_row)
  }
  
  x <- left_join(r2z_CP, growth_OP, , by = c("OPEN_PROM", "OPEN-PROM Subsector Emission Growth Rates", "period"))
  
  x <- x %>%
    arrange(`OPEN_PROM`, `OPEN-PROM Subsector Emission Growth Rates`, company_id, period) %>%
    group_by(`OPEN_PROM`, `OPEN-PROM Subsector Emission Growth Rates`, company_id) %>%
    rename(`Growth Rates` = emissions.y) %>%
    mutate(
      # emissionsGR: first value based on first emissions.x, then recursive by lag
      emissionsGR = {
        v <- rep(NA_real_, n())  # initialize
        if(!all(is.na(emissions.x))) {
          v[1] <- emissions.x[1]
          for(i in 2:n()) {
            if(!is.na(emissions.x[i])) {
              v[i] <- v[i-1] * (1 + `Growth Rates`[i] / 100)
            }
          }
        }
        v
      },
      
      # emissions.lowest: take the lower of emissions.x and emissionsGR
      emissions.lowest = pmin(emissions.x, emissionsGR, na.rm = TRUE),
      
      # emissions.final: recursive calculation only if emissions.lowest is NA
      emissions.final = {
        v <- emissions.lowest
        for(i in 1:length(v)) {
          if(is.na(v[i])) {
            if(i > 1 && !is.na(v[i-1])) {
              v[i] <- v[i-1] * (1 + `Growth Rates`[i] / 100)
            }
          }
          # else keep emissions.lowest (including 0)
        }
        v
      }
    ) %>%
    ungroup()
  
  target_cols <- paste0("target_year_", 1:5)
  
  # Calculate sum of rows where emissions.final < emissionsGR for all target years
  total_negative <- sum(map_int(target_cols, function(col) {
    temp <- x %>%
      select(emissions.final, emissionsGR, all_of(col)) %>%
      mutate(final_GR = if_else(!is.na(.data[[col]]), emissions.final - emissionsGR, NA_real_))
    
    sum(temp$final_GR < 0, na.rm = TRUE)
  }))
  
  write.csv(data.frame(total_negative = total_negative), "total_negative.csv", row.names = FALSE)
  
  
  # total <- select(x, c("OPEN-PROM Subsector Emission Growth Rates", "period", "emissions.final", "OPEN_PROM")) %>%
  #   group_by(`OPEN-PROM Subsector Emission Growth Rates`, period, OPEN_PROM) %>%
  #   mutate(final_aggr = sum(emissions.final, na.rm = TRUE)) %>%
  #   ungroup()
  
  total <- select(x, c("OPEN-PROM Subsector Emission Growth Rates", "period", "emissions.final", "OPEN_PROM")) %>%
    group_by(`OPEN-PROM Subsector Emission Growth Rates`, period, OPEN_PROM) %>%  # group by all other columns
    summarise(final_aggr = sum(emissions.final, na.rm = TRUE), .groups = "drop")
  
  total_wide <- total %>%
    pivot_wider(
      names_from = period,       # the column whose values become new columns
      values_from = final_aggr   # the values to fill in
    )
  
  write.csv(total_wide, "total_wide.csv", row.names = TRUE)
  
  setwd("C:/Users/sioutas/Ricardo Plc/Global Integrated Assessment Models - Documents/Work/PROMETHEUS Model/madratverse/sources/ACHIEVE")
  
  # List all .xlsx files in the folder
  files <- list.files(pattern = "\\.xlsx$", full.names = TRUE)
  # files <- c(
  #   "./GrowthRateVariables.xlsx",
  #   "./r2z_Mapping.xlsx",
  #   "./r2z_regions_ambition_pathways_2024.xlsx"
  # )
  
  files <- c(
    "./GrowthRateVariables.xlsx",
    "./Sbti_mapping.xlsx",
    "./sbti_regions_ambition_pathways_2024.xlsx"
  )
  
  # Read all sheets from all files into a flat list
  data_list <- map(files, function(file) {
    
    # Get sheet names in the current Excel file
    sheets <- excel_sheets(file)
    
    # Read all sheets and name them file_sheet
    map(sheets, function(sheet) {
      read_excel(file, sheet = sheet)
    }) |> 
      set_names(paste0(tools::file_path_sans_ext(basename(file)), "_", sheets))
    
  }) |> 
    flatten()  # flatten to a single list
  
  for (i in seq_along(data_list)) {
    assign(names(data_list)[i], data_list[[i]])
  }
  
  # r2z_regions_ambition_pathways_2024_Target_CP
  Target_CP <- sbti_regions_ambition_pathways_2024_Target_CP %>%
    pivot_longer(
      cols = `2023`:`2099`,
      names_to = "period",
      values_to = "value"
    )  %>% rename(`ISO Code` = country_of_emissions) %>%
    rename(`Sbti Sectors` = sector)
  
  # r2z_CP <- left_join(Target_CP, r2z_Mapping_Regional, by = "CDP_ISO")
  # r2z_CP <- left_join(r2z_CP, r2z_Mapping_Sectoral, by = "R2Z sector")
  r2z_CP <- left_join(Target_CP, Sbti_mapping_Regional, by = "ISO Code")
  r2z_CP <- left_join(r2z_CP, Sbti_mapping_Sectoral, by = "Sbti Sectors")
  
  # List of target year columns
  target_cols <- paste0("target_year_", 1:5)
  
  r2z_CP <- r2z_CP[,] %>%
    mutate(
      period = as.numeric(period),
      across(all_of(target_cols), as.numeric)
    ) %>%
    rowwise() %>%  # process row by row
    mutate(
      max_target = max(c_across(all_of(target_cols)), na.rm = TRUE),  # max target ignoring NA
      emissions = if(!is.na(max_target) & period <= max_target) value else NA_real_
    ) %>%
    ungroup() %>%
    select(-max_target) 
  
  growth_OP <- GrowthRateVariables_GrowthRateVariables[,c("region", "variable", "period",  "value")]
  names(growth_OP) <- c("OPEN-PROM Region Code", "OPEN-PROM sectors", "period", "emissions")
  
  if(!2100 %in% r2z_CP$period) {
    
    # Get the last year (2099) row(s)
    last_year_row <- r2z_CP %>% filter(period == max(period))
    
    # Copy and set period to 2100
    new_row <- last_year_row %>% mutate(period = 2100)
    
    # Bind the new row to the dataset
    r2z_CP <- bind_rows(r2z_CP, new_row)
  }
  
  x <- left_join(r2z_CP, growth_OP, , by = c("OPEN-PROM Region Code", "OPEN-PROM sectors", "period"))
  
  x <- x %>%
    arrange(`OPEN-PROM Region Code`, `OPEN-PROM sectors`, company_id, period) %>%
    group_by(`OPEN-PROM Region Code`, `OPEN-PROM sectors`, company_id) %>%
    rename(`Growth Rates` = emissions.y) %>%
    mutate(
      # emissionsGR: first value based on first emissions.x, then recursive by lag
      emissionsGR = {
        v <- rep(NA_real_, n())  # initialize
        if(!all(is.na(emissions.x))) {
          v[1] <- emissions.x[1]
          for(i in 2:n()) {
            if(!is.na(emissions.x[i])) {
              v[i] <- v[i-1] * (1 + `Growth Rates`[i] / 100)
            }
          }
        }
        v
      },
      
      # emissions.lowest: take the lower of emissions.x and emissionsGR
      emissions.lowest = pmin(emissions.x, emissionsGR, na.rm = TRUE),
      
      # emissions.final: recursive calculation only if emissions.lowest is NA
      emissions.final = {
        v <- emissions.lowest
        for(i in 1:length(v)) {
          if(is.na(v[i])) {
            if(i > 1 && !is.na(v[i-1])) {
              v[i] <- v[i-1] * (1 + `Growth Rates`[i] / 100)
            }
          }
          # else keep emissions.lowest (including 0)
        }
        v
      }
    ) %>%
    ungroup()
  
  target_cols <- paste0("target_year_", 1:5)
  
  # Calculate sum of rows where emissions.final < emissionsGR for all target years
  total_negative <- sum(map_int(target_cols, function(col) {
    temp <- x %>%
      select(emissions.final, emissionsGR, all_of(col)) %>%
      mutate(final_GR = if_else(!is.na(.data[[col]]), emissions.final - emissionsGR, NA_real_))
    
    sum(temp$final_GR < 0, na.rm = TRUE)
  }))
  
  # Sum of non-NA rows across all 5 target years
  total_non_na <- sum(map_int(target_cols, function(col) sum(!is.na(x[[col]]))))
  
  write.csv(data.frame(total_non_na = total_non_na), "total_non_na_Sbti.csv", row.names = FALSE)
  
  write.csv(data.frame(total_negative = total_negative), "total_negative_Sbti.csv", row.names = FALSE)
  
  
  # total <- select(x, c("OPEN-PROM Subsector Emission Growth Rates", "period", "emissions.final", "OPEN_PROM")) %>%
  #   group_by(`OPEN-PROM Subsector Emission Growth Rates`, period, OPEN_PROM) %>%
  #   mutate(final_aggr = sum(emissions.final, na.rm = TRUE)) %>%
  #   ungroup()
  
  total <- select(x, c("OPEN-PROM sectors", "period", "emissions.final", "OPEN-PROM Region Code")) %>%
    group_by(`OPEN-PROM sectors`, period, `OPEN-PROM Region Code`) %>%  # group by all other columns
    summarise(final_aggr = sum(emissions.final, na.rm = TRUE), .groups = "drop")
  
  total_wide <- total %>%
    pivot_wider(
      names_from = period,       # the column whose values become new columns
      values_from = final_aggr   # the values to fill in
    )
  
  write.csv(total_wide, "total_wide_Sbti.csv", row.names = TRUE)
  
  return(x)
  
}

setwd("C:/Users/sioutas/Ricardo Plc/Global Integrated Assessment Models - Documents/Work/PROMETHEUS Model/madratverse/sources/ACHIEVE")

# List all .xlsx files in the folder
files <- list.files(pattern = "\\.xlsx$", full.names = TRUE)

files <- c(
  "./GrowthRateVariables.xlsx",
  "./CDP_mappings.xlsx",
  "./all_regions_ambition_pathways_2024.xlsx"
)

# Read all sheets from all files into a flat list
data_list <- map(files, function(file) {
  
  # Get sheet names in the current Excel file
  sheets <- excel_sheets(file)
  
  # Read all sheets and name them file_sheet
  map(sheets, function(sheet) {
    read_excel(file, sheet = sheet)
  }) |> 
    set_names(paste0(tools::file_path_sans_ext(basename(file)), "_", sheets))
  
}) |> 
  flatten()  # flatten to a single list

for (i in seq_along(data_list)) {
  assign(names(data_list)[i], data_list[[i]])
}

# r2z_regions_ambition_pathways_2024_Target_CP
Target_CP <- all_regions_ambition_pathways_2024_Target_CP %>%
  pivot_longer(
    cols = `2023`:`2099`,
    names_to = "period",
    values_to = "value"
  )  %>% rename(`CDP_ISO` = country_of_emissions) %>%
  rename(`R2Z sector` = sector)

# r2z_CP <- left_join(Target_CP, r2z_Mapping_Regional, by = "CDP_ISO")
# r2z_CP <- left_join(r2z_CP, r2z_Mapping_Sectoral, by = "R2Z sector")
r2z_CP <- left_join(Target_CP, CDP_mappings_regional, by = "CDP_ISO")
r2z_CP <- left_join(r2z_CP, CDP_mappings_sectoral, by = "R2Z sector")

# List of target year columns
target_cols <- paste0("target_year_", 1:5)

r2z_CP <- r2z_CP %>%
  mutate(
    period = as.numeric(period),
    across(all_of(target_cols), as.numeric)
  ) %>%
  rowwise() %>%  # process row by row
  mutate(
    max_target = max(c_across(all_of(target_cols)), na.rm = TRUE),  # max target ignoring NA
    emissions = if(!is.na(max_target) & period <= max_target) value else NA_real_
  ) %>%
  ungroup() %>%
  select(-max_target) 

growth_OP <- GrowthRateVariables_GrowthRateVariables[,c("region", "variable", "period",  "value")]
names(growth_OP) <- c("OPEN_PROM_region", "OPEN_PROM_sector", "period", "emissions")

if(!2100 %in% r2z_CP$period) {
  
  # Get the last year (2099) row(s)
  last_year_row <- r2z_CP %>% filter(period == max(period))
  
  # Copy and set period to 2100
  new_row <- last_year_row %>% mutate(period = 2100)
  
  # Bind the new row to the dataset
  r2z_CP <- bind_rows(r2z_CP, new_row)
}

x <- left_join(r2z_CP, growth_OP, , by = c("OPEN_PROM_region", "OPEN_PROM_sector", "period"))

x <- x %>%
  arrange(`OPEN_PROM_region`, `OPEN_PROM_sector`, company_id, period) %>%
  group_by(`OPEN_PROM_region`, `OPEN_PROM_sector`, company_id) %>%
  rename(`Growth Rates` = emissions.y) %>%
  mutate(
    # emissionsGR: first value based on first emissions.x, then recursive by lag
    emissionsGR = {
      v <- rep(NA_real_, n())  # initialize
      if(!all(is.na(emissions.x))) {
        v[1] <- emissions.x[1]
        for(i in 2:n()) {
          if(!is.na(emissions.x[i])) {
            v[i] <- v[i-1] * (1 + `Growth Rates`[i] / 100)
          }
        }
      }
      v
    },
    
    # emissions.lowest: take the lower of emissions.x and emissionsGR
    emissions.lowest = pmin(emissions.x, emissionsGR, na.rm = TRUE),
    
    # emissions.final: recursive calculation only if emissions.lowest is NA
    emissions.final = {
      v <- emissions.lowest
      for(i in 1:length(v)) {
        if(is.na(v[i])) {
          if(i > 1 && !is.na(v[i-1])) {
            v[i] <- v[i-1] * (1 + `Growth Rates`[i] / 100)
          }
        }
        # else keep emissions.lowest (including 0)
      }
      v
    }
  ) %>%
  ungroup()

target_cols <- paste0("target_year_", 1:5)

# Calculate sum of rows where emissions.final < emissionsGR for all target years
total_negative <- sum(map_int(target_cols, function(col) {
  temp <- x %>%
    select(emissions.final, emissionsGR, all_of(col)) %>%
    mutate(final_GR = if_else(!is.na(.data[[col]]), emissions.final - emissionsGR, NA_real_))
  
  sum(temp$final_GR < 0, na.rm = TRUE)
}))

# Sum of non-NA rows across all 5 target years
total_non_na <- sum(map_int(target_cols, function(col) sum(!is.na(x[[col]]))))

write.csv(data.frame(total_non_na = total_non_na), "non_na_CDP_FinalPathways.csv", row.names = FALSE)

write.csv(data.frame(total_negative = total_negative), "total_negative_CDP_FinalPathways.csv", row.names = FALSE)


# total <- select(x, c("OPEN-PROM Subsector Emission Growth Rates", "period", "emissions.final", "OPEN_PROM")) %>%
#   group_by(`OPEN-PROM Subsector Emission Growth Rates`, period, OPEN_PROM) %>%
#   mutate(final_aggr = sum(emissions.final, na.rm = TRUE)) %>%
#   ungroup()

total <- select(x, c("OPEN_PROM_sector", "period", "emissions.final", "OPEN_PROM_region")) %>%
  group_by(`OPEN_PROM_sector`, period, `OPEN_PROM_region`) %>%  # group by all other columns
  summarise(final_aggr = sum(emissions.final, na.rm = TRUE), .groups = "drop")

total_wide <- total %>%
  pivot_wider(
    names_from = period,       # the column whose values become new columns
    values_from = final_aggr   # the values to fill in
  )

write.csv(total_wide, "CDP_FinalPathways.csv", row.names = TRUE)












setwd("C:/Users/sioutas/Ricardo Plc/Global Integrated Assessment Models - Documents/Work/PROMETHEUS Model/madratverse/sources/ACHIEVE")

# List all .xlsx files in the folder
files <- list.files(pattern = "\\.xlsx$", full.names = TRUE)

files <- c(
  "./GrowthRateVariables.xlsx",
  "./Cities_mappings.xlsx",
  "./cities_ambition_pathways.xlsx"
)

# Read all sheets from all files into a flat list
data_list <- map(files, function(file) {
  
  # Get sheet names in the current Excel file
  sheets <- excel_sheets(file)
  
  # Read all sheets and name them file_sheet
  map(sheets, function(sheet) {
    read_excel(file, sheet = sheet)
  }) |> 
    set_names(paste0(tools::file_path_sans_ext(basename(file)), "_", sheets))
  
}) |> 
  flatten()  # flatten to a single list

for (i in seq_along(data_list)) {
  assign(names(data_list)[i], data_list[[i]])
}

# r2z_regions_ambition_pathways_2024_Target_CP
Target_CP <- cities_ambition_pathways_Target_CP %>%
  pivot_longer(
    cols = `2025`:`2100`,
    names_to = "period",
    values_to = "value"
  )  %>% rename(`ISO` = iso) %>%
  rename(`TIMER sectors` = TIMER_sector)

r2z_CP <- left_join(Target_CP, Cities_mappings_Regional, by = "ISO")
r2z_CP <- left_join(r2z_CP, Cities_mappings_Sectoral, by = "TIMER sectors")

# List of target year columns
target_cols <- paste0("T",1:6,"_target_year")

r2z_CP <- r2z_CP %>%
  mutate(
    period = as.numeric(period),
    across(all_of(target_cols), as.numeric)
  ) %>%
  rowwise() %>%  # process row by row
  mutate(
    max_target = max(c_across(all_of(target_cols)), na.rm = TRUE),  # max target ignoring NA
    emissions = if(!is.na(max_target) & period <= max_target) value else NA_real_
  ) %>%
  ungroup() %>%
  select(-max_target) 

growth_OP <- GrowthRateVariables_GrowthRateVariables[,c("region", "variable", "period",  "value")]
names(growth_OP) <- c("OPEN-PROM Region", "OPEN-PROM sectors", "period", "emissions")

if(!2023 %in% r2z_CP$period) {
  
  # Get the last year (2099) row(s)
  last_year_row <- r2z_CP %>% filter(period == min(period))
  
  # Copy and set period to 2023
  new_row <- last_year_row %>% mutate(period = 2023)
  
  # Bind the new row to the dataset
  r2z_CP <- bind_rows(r2z_CP, new_row)
}

if(!2024 %in% r2z_CP$period) {
  
  # Get the last year (2099) row(s)
  last_year_row <- r2z_CP %>% filter(period == min(period))
  
  # Copy and set period to 2024
  new_row <- last_year_row %>% mutate(period = 2024)
  
  # Bind the new row to the dataset
  r2z_CP <- bind_rows(r2z_CP, new_row)
}

x <- left_join(r2z_CP, growth_OP, , by = c("OPEN-PROM Region", "OPEN-PROM sectors", "period"))

x <- x %>%
  arrange(`OPEN-PROM Region`, `OPEN-PROM sectors`, orig_id, period) %>%
  group_by(`OPEN-PROM Region`, `OPEN-PROM sectors`, orig_id) %>%
  rename(`Growth Rates` = emissions.y) %>%
  mutate(
    # emissionsGR: first value based on first emissions.x, then recursive by lag
    emissionsGR = {
      v <- rep(NA_real_, n())  # initialize
      if(!all(is.na(emissions.x))) {
        v[1] <- emissions.x[1]
        for(i in 2:n()) {
          if(!is.na(emissions.x[i])) {
            v[i] <- v[i-1] * (1 + `Growth Rates`[i] / 100)
          }
        }
      }
      v
    },
    
    # emissions.lowest: take the lower of emissions.x and emissionsGR
    emissions.lowest = pmin(emissions.x, emissionsGR, na.rm = TRUE),
    
    # emissions.final: recursive calculation only if emissions.lowest is NA
    emissions.final = {
      v <- emissions.lowest
      for(i in 1:length(v)) {
        if(is.na(v[i])) {
          if(i > 1 && !is.na(v[i-1])) {
            v[i] <- v[i-1] * (1 + `Growth Rates`[i] / 100)
          }
        }
        # else keep emissions.lowest (including 0)
      }
      v
    }
  ) %>%
  ungroup()

# List of target year columns
target_cols <- paste0("T",1:6,"_target_year")

# Calculate sum of rows where emissions.final < emissionsGR for all target years
total_negative <- sum(map_int(target_cols, function(col) {
  temp <- x %>%
    select(emissions.final, emissionsGR, all_of(col)) %>%
    mutate(final_GR = if_else(!is.na(.data[[col]]), emissions.final - emissionsGR, NA_real_))
  
  sum(temp$final_GR < 0, na.rm = TRUE)
}))

# Sum of non-NA rows across all 5 target years
total_non_na <- sum(map_int(target_cols, function(col) sum(!is.na(x[[col]]))))

non_na_negative_cities_ambition_pathways <- rbind(total_non_na, total_negative)

write.csv(data.frame(non_na_negative_cities_ambition_pathways = non_na_negative_cities_ambition_pathways), "non_na_negative_cities_ambition_pathways.csv", row.names = FALSE)


# total <- select(x, c("OPEN-PROM Subsector Emission Growth Rates", "period", "emissions.final", "OPEN_PROM")) %>%
#   group_by(`OPEN-PROM Subsector Emission Growth Rates`, period, OPEN_PROM) %>%
#   mutate(final_aggr = sum(emissions.final, na.rm = TRUE)) %>%
#   ungroup()

total <- select(x, c("OPEN-PROM sectors", "period", "emissions.final", "OPEN-PROM Region")) %>%
  group_by(`OPEN-PROM sectors`, period, `OPEN-PROM Region`) %>%  # group by all other columns
  summarise(final_aggr = sum(emissions.final, na.rm = TRUE), .groups = "drop")

total_wide <- total %>%
  pivot_wider(
    names_from = period,       # the column whose values become new columns
    values_from = final_aggr   # the values to fill in
  )

write.csv(total_wide, "cities_ambition_pathways.csv", row.names = TRUE)

