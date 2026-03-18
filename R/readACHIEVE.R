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
  "./ReportEmiss.xlsx",
  "./CDP_mappings.xlsx",
  "./all_regions_ambition_pathways_2024.xlsx",
  "./OPEN-PROM_EmiSBS.xlsx"
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

# all_regions_ambition_pathways_2024_Target_CP <- filter(all_regions_ambition_pathways_2024_Target_CP, country_of_emissions == "FIN")

# r2z_regions_ambition_pathways_2024_Target_CP
Target_CP <- all_regions_ambition_pathways_2024_Target_CP %>%
  pivot_longer(
    cols = `2023`:`2099`,
    names_to = "period",
    values_to = "value"
  )  %>% rename(`CDP_ISO` = country_of_emissions) %>%
  rename(`R2Z sector` = sector) %>% filter(CDP_ISO %in% c("IND"))

# r2z_CP <- left_join(Target_CP, r2z_Mapping_Regional, by = "CDP_ISO")
# r2z_CP <- left_join(r2z_CP, r2z_Mapping_Sectoral, by = "R2Z sector")
r2z_CP <- left_join(Target_CP, CDP_mappings_regional, by = "CDP_ISO")
r2z_CP <- left_join(r2z_CP, CDP_mappings_sectoral, by = "R2Z sector")

# List of target year columns
target_cols <- paste0("target_year_", 1:5)

# r2z_CP <- r2z_CP %>%
#   mutate(
#     period = as.numeric(period),
#     across(all_of(target_cols), as.numeric)
#   ) %>%
#   rowwise() %>%  # process row by row
#   mutate(
#     max_target = max(c_across(all_of(target_cols)), na.rm = TRUE),  # max target ignoring NA
#     emissions = if(!is.na(max_target) & period <= max_target) value else NA_real_
#   ) %>%
#   ungroup() %>%
#   select(-max_target)

r2z_CP <- r2z_CP %>%
  mutate(
    period = as.numeric(period),
    across(all_of(target_cols), as.numeric)
  ) %>%
  rowwise() %>%
  mutate(
    max_target = max(c_across(all_of(target_cols)), na.rm = TRUE),
    emissions = if (!is.na(max_target) & period <= max_target) value else NA_real_
  ) %>%
  ungroup() %>%
  arrange(OPEN_PROM_region, OPEN_PROM_sector_emi, company_id, period) %>%
  
  # Step 1: carry forward emissions
  group_by(OPEN_PROM_region, OPEN_PROM_sector_emi, company_id) %>%
  fill(emissions, .direction = "down") %>%
  
  # Step 2: fallback → use last non-NA value from `value`
  mutate(
    value_filled = ifelse(is.na(value), NA_real_, value)
  ) %>%
  fill(value_filled, .direction = "down") %>%
  mutate(
    emissions = ifelse(is.na(emissions), value_filled, emissions)
  ) %>%
  ungroup() %>%
  select(-max_target, -value_filled)

NDC_Report <- ReportEmiss_Sheet1 %>%
  pivot_longer(
    cols = `2010`:`2100`,
    names_to = "period",
    values_to = "value"
  )
NDC_Report <- NDC_Report[,c("region", "variable", "period",  "value")] %>%
  mutate(period = as.numeric(period))
names(NDC_Report) <- c("OPEN_PROM_region", "OPEN_PROM_sector_emi", "period", "emissions")

if(!2100 %in% r2z_CP$period) {
  
  # Get the last year (2099) row(s)
  last_year_row <- r2z_CP %>% filter(period == max(period))
  
  # Copy and set period to 2100
  new_row <- last_year_row %>% mutate(period = 2100)
  
  # Bind the new row to the dataset
  r2z_CP <- bind_rows(r2z_CP, new_row)
}

ff <- r2z_CP %>%
  select(OPEN_PROM_region, OPEN_PROM_sector_emi, company_id, period, emissions)

ff_summary <- ff %>%
  group_by(OPEN_PROM_region, OPEN_PROM_sector_emi, period) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop")

NDC_Report <- NDC_Report %>%
  group_by(OPEN_PROM_region, OPEN_PROM_sector_emi) %>%
  mutate(
    emissions_2024 = emissions[period == 2024][1],
    emissions = ifelse(
      period == 2023 & emissions == 0 & !is.na(emissions_2024),
      emissions_2024,
      emissions
    )
  ) %>%
  select(-emissions_2024) %>%
  ungroup()

x <- left_join(ff_summary, NDC_Report, , by = c("OPEN_PROM_region", "OPEN_PROM_sector_emi", "period"))

# 3. Arrange, group, and compute new column
result <- x %>%
  arrange(OPEN_PROM_region, OPEN_PROM_sector_emi, period) %>%
  group_by(OPEN_PROM_region, OPEN_PROM_sector_emi) %>%
  mutate(
    first_emissions_x = first(emissions.x),
    first_emissions_y = first(emissions.y),
    scale_factor = first_emissions_x / first_emissions_y,
    amb_path = emissions.x
  ) %>%
  mutate(
    amb_path = {
      n <- length(amb_path)
      fin <- amb_path
      for(i in 2:n) {
        if(!is.na(fin[i-1]) && !is.na(emissions.y[i]) && !is.na(emissions.y[i-1]) && !is.na(scale_factor[1]))
        {
          new_val <- fin[i-1] + (emissions.y[i] - emissions.y[i-1]) * scale_factor[1]
          if(is.na(fin[i]))
          {
            fin[i] <- new_val
          } else if(!is.na(new_val) && new_val < fin[i])
          {
            fin[i] <- new_val
          }
        }
      }
      pmax(fin, 0)   # 👈 THIS enforces non-negative values
    }
  ) %>%
  ungroup()

result <- result %>%
  arrange(OPEN_PROM_region, OPEN_PROM_sector_emi, period) %>%
  group_by(OPEN_PROM_region, OPEN_PROM_sector_emi) %>%
  mutate(
    NDC_Comp = {
      n <- length(amb_path)
      NDC_Comp <- numeric(n)         # create NDC_Comp
      NDC_Comp[1] <- amb_path[1]     # first value
      
      for(i in 2:n) {
        # compute increment only if previous and required values exist
        if(!is.na(NDC_Comp[i-1]) && !is.na(emissions.y[i]) && !is.na(emissions.y[i-1]) &&
           !is.na(emissions.y[1]) && !is.na(NDC_Comp[1])) {
          NDC_Comp[i] <- NDC_Comp[i-1] + (emissions.y[i] - emissions.y[i-1]) * NDC_Comp[1] / emissions.y[1]
        } else {
          NDC_Comp[i] <- NA  # propagate NA if any needed value is NA
        }
      }
      NDC_Comp
    }
  ) %>%
  ungroup()

names(result) <- gsub("emissions.y", "NDC", names(result))

# result <- result %>%
#   group_by(OPEN_PROM_region, OPEN_PROM_sector_emi, company_id) %>%  # if you want group-wise
#   mutate(
#     NDC_CC = if_else(
#       row_number() == 1,                     # first row in each group
#       first(NDC),                            # assign first NDC
#       NDC - (NDC_Comp - amb_path) / 1e6     # rest of the rows
#     )
#   ) %>%
#   ungroup()
# 
# result <- result %>%
#   arrange(OPEN_PROM_region, OPEN_PROM_sector_emi, company_id, period) %>%
#   group_by(OPEN_PROM_region, OPEN_PROM_sector_emi, company_id) %>%
#   mutate(
#     SCC = if_else(
#       row_number() == 1,
#       first(NDC),
#       NDC + (amb_path - lag(amb_path)) * first(NDC) / first(amb_path)
#     )
#   ) %>%
#   ungroup()

result <- result %>%
  arrange(OPEN_PROM_region, OPEN_PROM_sector_emi, period) %>%
  group_by(OPEN_PROM_region, OPEN_PROM_sector_emi) %>%
  mutate(
    NDC_CC = if_else(
      row_number() == 1,
      first(NDC),
      NDC - (NDC_Comp - amb_path) / 1e6
    ),
    
    SCC = {
      n <- n()
      scc <- numeric(n)
      
      scc[1] <- first(NDC)  # first row
      
      for(i in 2:n) {
        if(!is.na(amb_path[i]) && !is.na(amb_path[i-1]) &&
           !is.na(first(NDC)) && !is.na(first(amb_path))) {
          
          scc[i] <- scc[i-1] + 
            (amb_path[i] - amb_path[i-1]) * first(NDC) / first(amb_path)
        } else {
          scc[i] <- NA
        }
      }
      
      scc
    }
  ) %>%
  ungroup()

NDC_Report
all_var <- `OPEN-PROM_EmiSBS_OPEN-PROM_EmiSBS`
restSBS <- setdiff(all_var$`OPEN-PROM_emissions`, result$OPEN_PROM_sector_emi)
restSBSEmi <- filter(NDC_Report, OPEN_PROM_sector_emi %in% restSBS)

restSBSEmi_sum <- restSBSEmi %>%
  group_by(OPEN_PROM_region, period) %>%
  summarise(
    emissions = sum(emissions, na.rm = TRUE),
    .groups = "drop"
  )

result <- result %>%
  group_by(period, OPEN_PROM_region) %>%
  mutate(NDC_sum = sum(NDC, na.rm = TRUE)) %>%
  ungroup()

result <- result %>%
  group_by(period, OPEN_PROM_region) %>%
  mutate(NDC_CC_sum = sum(NDC_CC, na.rm = TRUE)) %>%
  ungroup()

result <- result %>%
  group_by(period, OPEN_PROM_region) %>%
  mutate(SCC_sum = sum(SCC, na.rm = TRUE)) %>%
  ungroup()


######################################
NDC_sum <- filter(result,
                       period %in% c(2023:2050)
) %>%
  select(OPEN_PROM_region, period, NDC_sum, OPEN_PROM_sector_emi) %>%
  rename(emissions = NDC_sum)

j <- filter(restSBSEmi, "")

test <- rbind(, )




















###########################################

final_result <- filter(result,
  period %in% c(2023:2050)
) %>%
  select(OPEN_PROM_region, period, NDC_sum, SCC_sum, NDC_CC_sum) %>% 
  distinct()

result_joined <- final_result %>%
  left_join(
    restSBSEmi_sum,
    by = c("OPEN_PROM_region", "period")
  )

result_joined_final <- result_joined %>%
  mutate(
    NDC_sum    = NDC_sum + emissions,
    SCC_sum    = SCC_sum + emissions,
    NDC_CC_sum = NDC_CC_sum + emissions
  )

# write.csv(result, "result.csv", row.names = TRUE)


plots <- result_joined_final

library(ggplot2)

plots %>%
  filter(
    OPEN_PROM_region %in% c("USA", "CHA", "LAM", "DEU", "JPN", "GBR", "REF", "FRA", "NEU"),
    period %in% c(2023:2050)
  ) %>%
  select(OPEN_PROM_region, period, NDC_sum, SCC_sum, NDC_CC_sum) %>% 
  distinct() %>%
  mutate(
    OPEN_PROM_region = recode(OPEN_PROM_region,
                              "USA" = "USA",
                              "CHA" = "China",
                              "LAM" = "Latin America",
                              "DEU" = "Germany",
                              "JPN" = "Japan",
                              "GBR" = "Great Britain",
                              "REF" = "Russia",
                              "FRA" = "France",
                              "NEU" = "Non-EU Europe"
    )
  ) %>%
  pivot_longer(
    cols = c(NDC_sum, SCC_sum, NDC_CC_sum),
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(x = period, y = value, color = variable, group = variable)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ OPEN_PROM_region, scales = "free") +
  labs(
    title = "",
    x = "Year",
    y = "Value",
    color = "Indicator"
  ) +
  theme_minimal()
ggsave("9.png", width = 10, height = 6, dpi = 300)


result_output <- result %>%
  select(OPEN_PROM_region, OPEN_PROM_sector_emi, period, amb_path, NDC, NDC_CC, SCC)

NDC_sum <- result %>%
  select(OPEN_PROM_region, period, NDC_sum)  %>% 
  distinct() %>%
  pivot_wider(
    names_from = period,   # each unique value of 'period' becomes a new column
    values_from = NDC_sum  # fill the new columns with values from 'NDC_sum'
  )

NDC_CC_sum <- result %>%
  select(OPEN_PROM_region, period, NDC_CC_sum) %>% 
  distinct()%>%
  pivot_wider(
    names_from = period,   # each unique value of 'period' becomes a new column
    values_from = NDC_CC_sum  # fill the new columns with values from 'NDC_sum'
  )

SCC_sum <- result %>%
  select(OPEN_PROM_region, period, SCC_sum) %>% 
  distinct()%>%
  pivot_wider(
    names_from = period,   # each unique value of 'period' becomes a new column
    values_from = SCC_sum  # fill the new columns with values from 'NDC_sum'
  )


library(openxlsx)

# Suppose your data frames are named like this
df_list <- list(result_output = result_output,
                NDC_sum = NDC_sum,
                SCC_sum = SCC_sum,
                NDC_CC_sum = NDC_CC_sum)  # replace with your actual 4th df

# Create a new workbook
wb <- createWorkbook()


# Loop over each df and add it as a sheet with the name of the df
for (df_name in names(df_list)) {
  addWorksheet(wb, sheetName = df_name)
  writeData(wb, sheet = df_name, df_list[[df_name]])
}

# Save the workbook
saveWorkbook(wb, file = "result_output.xlsx", overwrite = TRUE)

p <- plots %>%
  filter(
    period %in% 2023:2050
  ) %>%
  select(OPEN_PROM_region, period, NDC_sum, SCC_sum, NDC_CC_sum) %>%
  distinct() %>%
  pivot_longer(
    cols = c(NDC_sum, SCC_sum, NDC_CC_sum),
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(
    x = period,
    y = value,
    color = variable,
    group = variable
  )) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ OPEN_PROM_region, scales = "free_y") +  # <- key line
  labs(
    title = "All Regions",
    x = "Year",
    y = "Value",
    color = "Indicator"
  ) +
  theme_minimal()

# Save the plot
ggsave("all.png", plot = p, width = 12, height = 8, dpi = 300)



# 
# write.csv(ff_summary, "ff_summary.csv", row.names = TRUE)
# 
# names(amb_path) <- gsub("amb_path", "value", names(amb_path))


amb_path <- amb_path %>%
  group_by(period, OPEN_PROM_region, OPEN_PROM_sector_emi) %>%
  mutate(value_sum = sum(value, na.rm = TRUE)) %>%
  ungroup()

amb_path <- amb_path %>%
  group_by(period, OPEN_PROM_region, OPEN_PROM_sector_emi) %>%
  summarise(
    value_sum = sum(value, na.rm = TRUE),
    .groups = "drop"   # ungroup automatically
  )


amb_path_wide <- amb_path %>%
  pivot_wider(
    names_from = OPEN_PROM_region,  # column names come from regions
    values_from = value_sum,        # values come from the summed column
    values_fill = 0                 # optional: fill missing combinations with 0
  )

x <- x %>%
  arrange(`OPEN_PROM_region`, `OPEN_PROM_sector_emi`, company_id, period) %>%
  group_by(`OPEN_PROM_region`, `OPEN_PROM_sector_emi`, company_id) %>%
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


# data <- read.csv("CDP_FinalPathways.csv")




