#' readACHIEVE
#'
#' Read in a XLSX file from ACHIEVE and convert it to a magpie object.
#' 
#'
#' @return magpie object with the requested output data
#'
#' @author Fotis Sioutas, Dionysis
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
  

setwd("C:/Users/dp37/Ricardo Plc/Global Integrated Assessment Models - Documents/Work/PROMETHEUS Model/madratverse/sources/ACHIEVE")

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
    cols = `2024`:`2050`,
    names_to = "period",
    values_to = "value"
  )  %>% rename(`CDP_ISO` = country_of_emissions) %>%
  rename(`R2Z sector` = sector)  %>%
# %>% filter(CDP_ISO %in% c("AUT","DNK"))
  select(-any_of(c("2023", as.character(2050:2099))))

# r2z_CP <- left_join(Target_CP, r2z_Mapping_Regional, by = "CDP_ISO")
# r2z_CP <- left_join(r2z_CP, r2z_Mapping_Sectoral, by = "R2Z sector")
r2z_CP <- left_join(Target_CP, CDP_mappings_regional, by = "CDP_ISO")
r2z_CP <- left_join(r2z_CP, CDP_mappings_sectoral, by = "R2Z sector")

r2z_CP <- r2z_CP %>%
  mutate(
    OPEN_PROM_sector_emi = ifelse(
      scopes %in% c("S2M", "S2L"),
      "Gross Emissions|CO2|Energy|Supply|Electricity",
      OPEN_PROM_sector_emi
    )
  )

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
  arrange(OPEN_PROM_region, OPEN_PROM_sector_emi, company_id, period, scopes) %>%
  
  # Step 1: carry forward emissions
  group_by(OPEN_PROM_region, OPEN_PROM_sector_emi, company_id, scopes) %>%
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

# if(!2100 %in% r2z_CP$period) {
#   
#   # Get the last year (2099) row(s)
#   last_year_row <- r2z_CP %>% filter(period == max(period))
#   
#   # Copy and set period to 2100
#   new_row <- last_year_row %>% mutate(period = 2100)
#   
#   # Bind the new row to the dataset
#   r2z_CP <- bind_rows(r2z_CP, new_row)
# }

ff <- r2z_CP %>%
  select(OPEN_PROM_region, OPEN_PROM_sector_emi, company_id, period, emissions, scopes)

ff_summary <- ff %>%
  group_by(OPEN_PROM_region, OPEN_PROM_sector_emi, period) %>%
  summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop")

# NDC_Report <- NDC_Report %>%
#   group_by(OPEN_PROM_region, OPEN_PROM_sector_emi) %>%
#   mutate(
#     emissions_2024 = emissions[period == 2024][1],
#     emissions = ifelse(
#       period == 2023 & emissions == 0 & !is.na(emissions_2024),
#       emissions_2024,
#       emissions
#     )
#   ) %>%
#   select(-emissions_2024) %>%
#   ungroup()

x <- left_join(ff_summary, NDC_Report, , by = c("OPEN_PROM_region", "OPEN_PROM_sector_emi", "period"))

x <- x %>%
  group_by(OPEN_PROM_region, OPEN_PROM_sector_emi) %>%
  mutate(
    flag = {
      val_2024 <- emissions.x[period == 2024]
      ref_2024 <- emissions.y[period == 2024]
      any(val_2024 > ref_2024 * 1e6, na.rm = TRUE)
    },
    emissions.x = if_else(flag, 0, emissions.x)
  ) %>%
  ungroup() %>%
  select(-flag)

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
  group_by(OPEN_PROM_region, OPEN_PROM_sector_emi) %>%
  arrange(period) %>%
  mutate(rate = (NDC - lag(NDC)) / lag(NDC)) %>%
  mutate(
    inst = ifelse(period == 2024, NDC - amb_path / 1e6, NA),
  ) %>%
  
  mutate(inst = first(inst[period == 2024]))  %>%
  mutate(
    rate = ifelse(period == 2024, 0, rate),
    NDC_NoComp = cumprod(1+rate) * inst) %>%
  mutate(
    NDC_CC = NDC_NoComp + amb_path / 1e6,
  ) %>%
  ungroup()

result <- result %>%
  arrange(OPEN_PROM_region, OPEN_PROM_sector_emi, period) %>%
  group_by(OPEN_PROM_region, OPEN_PROM_sector_emi) %>%
  mutate(
    # SCC calculation remains unchanged
    SCC = {
      n <- n()
      scc <- numeric(n)
      scc[1] <- first(NDC)
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
NDC_sum_sub <- filter(result,
                       period %in% c(2024:2050)
) %>%
  select(OPEN_PROM_region, period, NDC, OPEN_PROM_sector_emi) %>%
  rename(emissions = NDC)

restSBSEmi <- filter(restSBSEmi, OPEN_PROM_region %in% unique(NDC_sum_sub[["OPEN_PROM_region"]]))

NDC_sum_sub <- rbind(NDC_sum_sub, restSBSEmi) %>% rename(value = emissions, region = OPEN_PROM_region, variable = OPEN_PROM_sector_emi)

NDC_sum_sub <- as.quitte(NDC_sum_sub)

NDC_CC_sum_sub <- filter(result,
                      period %in% c(2024:2050)
) %>%
  select(OPEN_PROM_region, period, NDC_CC, OPEN_PROM_sector_emi) %>%
  rename(emissions = NDC_CC)

NDC_CC_sum_sub <- rbind(NDC_CC_sum_sub, restSBSEmi) %>% rename(value = emissions, region = OPEN_PROM_region, variable = OPEN_PROM_sector_emi)

NDC_CC_sum_sub <- as.quitte(NDC_CC_sum_sub)


SCC_sum_sub <- filter(result,
                         period %in% c(2024:2050)
) %>%
  select(OPEN_PROM_region, period, SCC, OPEN_PROM_sector_emi) %>%
  rename(emissions = SCC)

SCC_sum_sub <- rbind(SCC_sum_sub, restSBSEmi) %>% rename(value = emissions, region = OPEN_PROM_region, variable = OPEN_PROM_sector_emi)

SCC_sum_sub <- as.quitte(SCC_sum_sub)

NDC_sum_sub <- as.magpie(NDC_sum_sub)
NDC_CC_sum_sub <- as.magpie(NDC_CC_sum_sub)
SCC_sum_sub <- as.magpie(SCC_sum_sub)

NDC_sum_sub[is.na(NDC_sum_sub)] <- 0
NDC_CC_sum_sub[is.na(NDC_CC_sum_sub)] <- 0
SCC_sum_sub[is.na(SCC_sum_sub)] <- 0

NDC_sum_sub <- helperAggregateLevel(NDC_sum_sub, level = 1, recursive = TRUE)
NDC_CC_sum_sub <- helperAggregateLevel(NDC_CC_sum_sub, level = 1, recursive = TRUE)
SCC_sum_sub <- helperAggregateLevel(SCC_sum_sub, level = 1, recursive = TRUE)

########## mif
NDC_sum_sub_GLO <- dimSums(NDC_sum_sub, 1, na.rm = TRUE)
getItems(NDC_sum_sub_GLO, 1) <- "World"
NDC_sum_sub2 <- mbind(NDC_sum_sub, NDC_sum_sub_GLO)
NDC_sum_sub2[,,"Emissions|CO2|Energy|Supply"] <- NDC_sum_sub2[,,"Emissions|CO2|Energy|Supply"] +
  NDC_sum_sub2[,,"Gross Emissions|CO2|Energy|Supply|Electricity"]
NDC_sum_sub2[,,"Emissions|CO2"] <- NDC_sum_sub2[,,"Emissions|CO2"] +
  NDC_sum_sub2[,,"Gross Emissions|CO2|Energy|Supply|Electricity"]

NDC_sum_sub2ResNCom <- NDC_sum_sub2[, , c("Emissions|CO2|Energy|Demand|Residential","Emissions|CO2|Energy|Demand|Commercial")]
NDC_sum_sub2ResNCom <- dimSums(NDC_sum_sub2ResNCom, dim = 3, na.rm = TRUE)
getItems(NDC_sum_sub2ResNCom, 3) <- "Emissions|CO2|Energy|Demand|Residential and Commercial"
NDC_sum_sub2 <- mbind(NDC_sum_sub2, NDC_sum_sub2ResNCom)

NDC_CC_sum_sub_GLO <- dimSums(NDC_CC_sum_sub, 1, na.rm = TRUE)
getItems(NDC_CC_sum_sub_GLO, 1) <- "World"
NDC_CC_sum_sub2 <- mbind(NDC_CC_sum_sub, NDC_CC_sum_sub_GLO)
NDC_CC_sum_sub2[,,"Emissions|CO2|Energy|Supply"] <- NDC_CC_sum_sub2[,,"Emissions|CO2|Energy|Supply"] +
  NDC_CC_sum_sub2[,,"Gross Emissions|CO2|Energy|Supply|Electricity"]
NDC_CC_sum_sub2[,,"Emissions|CO2"] <- NDC_CC_sum_sub2[,,"Emissions|CO2"] +
  NDC_CC_sum_sub2[,,"Gross Emissions|CO2|Energy|Supply|Electricity"]

NDC_CC_sum_sub2ResNCom <- NDC_CC_sum_sub2[, , c("Emissions|CO2|Energy|Demand|Residential","Emissions|CO2|Energy|Demand|Commercial")]
NDC_CC_sum_sub2ResNCom <- dimSums(NDC_CC_sum_sub2ResNCom, dim = 3, na.rm = TRUE)
getItems(NDC_CC_sum_sub2ResNCom, 3) <- "Emissions|CO2|Energy|Demand|Residential and Commercial"
NDC_CC_sum_sub2 <- mbind(NDC_CC_sum_sub2, NDC_CC_sum_sub2ResNCom)

SCC_sum_sub_GLO <- dimSums(SCC_sum_sub, 1, na.rm = TRUE)
getItems(SCC_sum_sub_GLO, 1) <- "World"
SCC_sum_sub2 <- mbind(SCC_sum_sub, SCC_sum_sub_GLO)
SCC_sum_sub2[,,"Emissions|CO2|Energy|Supply"] <- SCC_sum_sub2[,,"Emissions|CO2|Energy|Supply"] +
  SCC_sum_sub2[,,"Gross Emissions|CO2|Energy|Supply|Electricity"]
SCC_sum_sub2[,,"Emissions|CO2"] <- SCC_sum_sub2[,,"Emissions|CO2"] +
  SCC_sum_sub2[,,"Gross Emissions|CO2|Energy|Supply|Electricity"]

SCC_sum_sub2ResNCom <- SCC_sum_sub2[, , c("Emissions|CO2|Energy|Demand|Residential","Emissions|CO2|Energy|Demand|Commercial")]
SCC_sum_sub2ResNCom <- dimSums(SCC_sum_sub2ResNCom, dim = 3, na.rm = TRUE)
getItems(SCC_sum_sub2ResNCom, 3) <- "Emissions|CO2|Energy|Demand|Residential and Commercial"
SCC_sum_sub2 <- mbind(SCC_sum_sub2, SCC_sum_sub2ResNCom)

write.report(NDC_sum_sub2, file = "all.mif",model = "OPEN-PROM",scenario = "NDC",append = FALSE,unit = "MtCO2/yr")
write.report(NDC_CC_sum_sub2, file = "all.mif",model = "OPEN-PROM",scenario = "NDC_CC",append = TRUE,unit = "MtCO2/yr")
write.report(SCC_sum_sub2, file = "all.mif",model = "OPEN-PROM",scenario = "SCC",append = TRUE,unit = "MtCO2/yr")
##############
find_percent <- r2z_CP[,c(1,4:7,10,14,17,20,23,26,36:37,42)]

# Step 1: pivot target years into long format
long_targets <- find_percent %>%
  pivot_longer(
    cols = starts_with("target_year_"),
    names_to = "source_target",
    values_to = "target_year"
  ) %>%
  filter(!is.na(target_year))

# Step 2: join with actual data (matching period)
result <- long_targets %>%
  left_join(
    find_percent %>%
      select(company_id, scopes, activity, period, emissions, value),
    by = c("company_id", "scopes", "activity")
  ) %>%
  filter(period == target_year) %>%
  mutate(diff = emissions - value)
total_non_na <- sum(map_int(target_cols, function(col) sum(!is.na(find_percent[[col]]))))

perc <- total_negative / total_non_na
###########################################

final_result <- filter(result,
  period %in% c(2024:2050)
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
    OPEN_PROM_region %in% c("AUT", "DNK"),
    period %in% c(2024:2050)
  ) %>%
  select(OPEN_PROM_region, period, NDC_sum, SCC_sum, NDC_CC_sum) %>% 
  distinct() %>%
  mutate(
    OPEN_PROM_region = recode(OPEN_PROM_region,
                              "AUT" = "Austria",
                              "DNK" = "Denmark"
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

# 
# result_output <- result %>%
#   select(OPEN_PROM_region, OPEN_PROM_sector_emi, period, amb_path, NDC, NDC_CC, SCC)
# 
# NDC_sum <- result %>%
#   select(OPEN_PROM_region, period, NDC_sum)  %>% 
#   distinct() %>%
#   pivot_wider(
#     names_from = period,   # each unique value of 'period' becomes a new column
#     values_from = NDC_sum  # fill the new columns with values from 'NDC_sum'
#   )
# 
# NDC_CC_sum <- result %>%
#   select(OPEN_PROM_region, period, NDC_CC_sum) %>% 
#   distinct()%>%
#   pivot_wider(
#     names_from = period,   # each unique value of 'period' becomes a new column
#     values_from = NDC_CC_sum  # fill the new columns with values from 'NDC_sum'
#   )
# 
# SCC_sum <- result %>%
#   select(OPEN_PROM_region, period, SCC_sum) %>% 
#   distinct()%>%
#   pivot_wider(
#     names_from = period,   # each unique value of 'period' becomes a new column
#     values_from = SCC_sum  # fill the new columns with values from 'NDC_sum'
#   )
# 
# 
# library(openxlsx)
# 
# # Suppose your data frames are named like this
# df_list <- list(result_output = result_output,
#                 NDC_sum = NDC_sum,
#                 SCC_sum = SCC_sum,
#                 NDC_CC_sum = NDC_CC_sum)  # replace with your actual 4th df
# 
# # Create a new workbook
# wb <- createWorkbook()
# 
# 
# # Loop over each df and add it as a sheet with the name of the df
# for (df_name in names(df_list)) {
#   addWorksheet(wb, sheetName = df_name)
#   writeData(wb, sheet = df_name, df_list[[df_name]])
# }
# 
# # Save the workbook
# saveWorkbook(wb, file = "result_output.xlsx", overwrite = TRUE)

p <- plots %>%
  filter(
    period %in% 2024:2050
  ) %>%
  select(OPEN_PROM_region, period, NDC_sum, SCC_sum, NDC_CC_sum) %>%
  distinct() %>%
  rename(NDC = NDC_sum, NDC_CC = NDC_CC_sum, SCC = SCC_sum)  %>%
  pivot_longer(
    cols = c(NDC, NDC_CC, SCC),
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




