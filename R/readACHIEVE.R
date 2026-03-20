#' readACHIEVE
#'
#' Read in a XLSX file from ACHIEVE and convert it to a magpie object.
#' 
#' @param subtype Companies or Cities
#' 
#' @return mif object with the requested output data
#'
#' @author Fotis Sioutas, Dionysis
#'
#' @examples
#' \dontrun{
#' a <- readSource("ACHIEVE", subtype = "Cities")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point facet_wrap labs theme_minimal ggsave
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @iimportFrom quitte as.quitte
#' @importFrom dplyr %>% filter select rename mutate summarise group_by ungroup arrange left_join distinct
#' @importFrom readxl read_excel excel_sheets
#' @importFrom tidyr  pivot_longer pivot_wider fill
#' @importFrom purrr flatten map
#' @importFrom postprom helperAggregateLevel write.report
#' @importFrom tools file_path_sans_ext
#'

readACHIEVE <- function(subtype = "Cities") {

  if (subtype == "Companies") {
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
      # filter(CDP_ISO %in% c("AUT", "DNK", "USA", "CHN")) %>% 
      select(-any_of(c("2023", as.character(2050:2099))))
    
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
    
    ff <- r2z_CP %>%
      select(OPEN_PROM_region, OPEN_PROM_sector_emi, company_id, period, emissions, scopes)
    
    ff_summary <- ff %>%
      group_by(OPEN_PROM_region, OPEN_PROM_sector_emi, period) %>%
      summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop")
    
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
    EU28 <- toolGetMapping(name = "EU28.csv",
                           type = "regional",
                           where = "mrprom")
    EU27 <- setdiff(EU28[["ISO3.Code"]],"GBR")
    NDC_sum_sub_GLO <- dimSums(NDC_sum_sub, 1, na.rm = TRUE)
    getItems(NDC_sum_sub_GLO, 1) <- "World"
    NDC_sum_sub_EU27 <- dimSums(NDC_sum_sub[EU27,,], 1, na.rm = TRUE)
    getItems(NDC_sum_sub_EU27, 1) <- "EU"
    NDC_sum_sub2 <- mbind(NDC_sum_sub, NDC_sum_sub_GLO, NDC_sum_sub_EU27)
    NDC_sum_sub2[,,"Emissions|CO2|Energy|Supply"] <- NDC_sum_sub2[,,"Emissions|CO2|Energy|Supply"] +
      NDC_sum_sub2[,,"Gross Emissions|CO2|Energy|Supply|Electricity"]
    NDC_sum_sub2[,,"Emissions|CO2"] <- NDC_sum_sub2[,,"Emissions|CO2"] +
      NDC_sum_sub2[,,"Gross Emissions|CO2|Energy|Supply|Electricity"]
    
    NDC_sum_sub2ResNCom <- NDC_sum_sub2[, , c("Emissions|CO2|Energy|Demand|Residential","Emissions|CO2|Energy|Demand|Commercial")]
    NDC_sum_sub2ResNCom <- dimSums(NDC_sum_sub2ResNCom, dim = 3, na.rm = TRUE)
    getItems(NDC_sum_sub2ResNCom, 3) <- "Emissions|CO2|Energy|Demand|Residential and Commercial"
    NDC_sum_sub2 <- mbind(NDC_sum_sub2, NDC_sum_sub2ResNCom)
    
    NDC_CC_sum_sub_EU27 <- dimSums(NDC_CC_sum_sub[EU27,,], 1, na.rm = TRUE)
    getItems(NDC_CC_sum_sub_EU27, 1) <- "EU"
    NDC_CC_sum_sub_GLO <- dimSums(NDC_CC_sum_sub, 1, na.rm = TRUE)
    getItems(NDC_CC_sum_sub_GLO, 1) <- "World"
    NDC_CC_sum_sub2 <- mbind(NDC_CC_sum_sub, NDC_CC_sum_sub_GLO, NDC_CC_sum_sub_EU27)
    NDC_CC_sum_sub2[,,"Emissions|CO2|Energy|Supply"] <- NDC_CC_sum_sub2[,,"Emissions|CO2|Energy|Supply"] +
      NDC_CC_sum_sub2[,,"Gross Emissions|CO2|Energy|Supply|Electricity"]
    NDC_CC_sum_sub2[,,"Emissions|CO2"] <- NDC_CC_sum_sub2[,,"Emissions|CO2"] +
      NDC_CC_sum_sub2[,,"Gross Emissions|CO2|Energy|Supply|Electricity"]
    
    NDC_CC_sum_sub2ResNCom <- NDC_CC_sum_sub2[, , c("Emissions|CO2|Energy|Demand|Residential","Emissions|CO2|Energy|Demand|Commercial")]
    NDC_CC_sum_sub2ResNCom <- dimSums(NDC_CC_sum_sub2ResNCom, dim = 3, na.rm = TRUE)
    getItems(NDC_CC_sum_sub2ResNCom, 3) <- "Emissions|CO2|Energy|Demand|Residential and Commercial"
    NDC_CC_sum_sub2 <- mbind(NDC_CC_sum_sub2, NDC_CC_sum_sub2ResNCom)
    
    SCC_sum_sub_EU27 <- dimSums(SCC_sum_sub[EU27,,], 1, na.rm = TRUE)
    getItems(SCC_sum_sub_EU27, 1) <- "EU"
    SCC_sum_sub_GLO <- dimSums(SCC_sum_sub, 1, na.rm = TRUE)
    getItems(SCC_sum_sub_GLO, 1) <- "World"
    SCC_sum_sub2 <- mbind(SCC_sum_sub, SCC_sum_sub_GLO, SCC_sum_sub_EU27)
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
    # Step 1: Select columns from r2z_CP
    percent_ambition <- r2z_CP %>%
      select(
        company_id,           # 1
        scopes,               # 10
        target_year_1,        # 17
        target_year_2,        # 20
        target_year_3,        # 23
        target_year_4,        # 26
        target_year_5,
        period,               # 36
        value,                # 37
        OPEN_PROM_region,     # 38
        OPEN_PROM_sector_emi  # 40
      )
    
    take_from_x <- x[,c(1,2,3,5)] %>%
      rename(value = emissions.y)
    
    # Step 2: Merge emissions.y from x based on region and sector
    percent_ambition <- percent_ambition %>%
      left_join(
        take_from_x,
        by = c("OPEN_PROM_region", "OPEN_PROM_sector_emi", "period")
      )
    
    percent_ambition <- percent_ambition %>%
      arrange(company_id, scopes, OPEN_PROM_region, OPEN_PROM_sector_emi, period) %>%
      group_by(company_id, scopes, OPEN_PROM_region, OPEN_PROM_sector_emi) %>%
      mutate(
        growth = (value.y - lag(value.y)) / lag(value.y),
        CompOP = {
          n <- n()
          out <- numeric(n)
          
          out[1] <- first(value.x)  # first value
          
          for (i in 2:n) {
            out[i] <- out[i-1] * (growth[i] + 1)
          }
          
          out
        }
      ) %>%
      ungroup()
    
    out_result <- percent_ambition %>%
      # Keep rows where any target year matches period
      filter(
        target_year_1 == period |
          target_year_2 == period |
          target_year_3 == period |
          target_year_4 == period |
          target_year_5 == period
      ) %>%
      # Group by company_id, OPEN_PROM_region, scopes, OPEN_PROM_sector_emi
      group_by(company_id, OPEN_PROM_region, scopes, OPEN_PROM_sector_emi) %>%
      # Count rows where value.x < CompOP
      summarise(rows_value_lt_emissions = sum(value.x < CompOP, na.rm = TRUE),
                total_rows = n(),
                .groups = "drop")
    
    total_sums <- out_result %>%
      summarise(
        sum_rows_value_lt_emissions = sum(rows_value_lt_emissions, na.rm = TRUE),
        sum_total_rows = sum(total_rows, na.rm = TRUE)
      )
    
    write.csv(total_sums, "All_Reg_Comp_Amb_percent.csv", row.names = TRUE)
    ###########################################
    All_Reg_Comp_Amb_percent <- result %>%
      select(OPEN_PROM_region, OPEN_PROM_sector_emi, period, NDC, NDC_CC, SCC)
    
    NDC_wide <- result %>%
      select(OPEN_PROM_region, period, NDC, OPEN_PROM_sector_emi)  %>% 
      distinct() %>%
      pivot_wider(
        names_from = period,   # each unique value of 'period' becomes a new column
        values_from = NDC  # fill the new columns with values from 'NDC_sum'
      )
    
    NDC_CC_wide <- result %>%
      select(OPEN_PROM_region, period, NDC_CC, OPEN_PROM_sector_emi) %>% 
      distinct()%>%
      pivot_wider(
        names_from = period,   # each unique value of 'period' becomes a new column
        values_from = NDC_CC  # fill the new columns with values from 'NDC_sum'
      )
    
    SCC_wide <- result %>%
      select(OPEN_PROM_region, period, SCC, OPEN_PROM_sector_emi) %>% 
      distinct()%>%
      pivot_wider(
        names_from = period,   # each unique value of 'period' becomes a new column
        values_from = SCC  # fill the new columns with values from 'NDC_sum'
      )
    
    library(openxlsx)
    df_list <- list(All_Reg_Comp_Amb_percent = All_Reg_Comp_Amb_percent,
                    NDC_wide = NDC_wide,
                    NDC_CC_wide = NDC_CC_wide,
                    SCC_wide = SCC_wide)  # replace with your actual 4th df
    wb <- createWorkbook()
    for (df_name in names(df_list)) {
      addWorksheet(wb, sheetName = df_name)
      writeData(wb, sheet = df_name, df_list[[df_name]])
    }
    
    saveWorkbook(wb, file = "ScenariosCompanies.xlsx", overwrite = TRUE)
    #####################################
    
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
  }



  if (subtype == "Cities") {
    ############## START Cities
    
    
    
    # List all .xlsx files in the folder
    files <- list.files(pattern = "\\.xlsx$", full.names = TRUE)
    
    files <- c(
      "./ReportEmiss.xlsx",
      "./Cities_mappings.xlsx",
      "./cities_ambition_pathways.xlsx",
      "./OPEN-PROM_EmiSBS_Cities.xlsx"
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
    
    Target_CP <- cities_ambition_pathways_Target_CP  %>%
      pivot_longer(
        cols = `2025`:`2050`,
        names_to = "period",
        values_to = "value"
      )  %>% rename(`CDP_ISO` = iso) %>%
      rename(`R2Z sector` = TIMER_sector)  %>%
      select(-any_of(c(as.character(2051:2100))))
    
    Target_CP <- select(Target_CP, - "target_sectors") %>% distinct()
    
    r2z_CP <- left_join(Target_CP, Cities_mappings_Regional, by = "CDP_ISO")
    r2z_CP <- left_join(r2z_CP, Cities_mappings_Sectoral, by = "R2Z sector")
    
    r2z_CP <- r2z_CP %>%
      mutate(
        OPEN_PROM_sector_emi = ifelse(
          target_scopes %in% c("2"),
          "Gross Emissions|CO2|Energy|Supply|Electricity",
          OPEN_PROM_sector_emi
        )
      )
    
    # List of target year columns
    target_cols <- paste0("T", 1:5,"_","target_year")
    
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
      arrange(OPEN_PROM_region, OPEN_PROM_sector_emi, climactor_id, period, target_scopes) %>%
      
      # Step 1: carry forward emissions
      group_by(OPEN_PROM_region, OPEN_PROM_sector_emi, climactor_id, target_scopes) %>%
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
    
    ff <- r2z_CP %>%
      select(OPEN_PROM_region, OPEN_PROM_sector_emi, climactor_id, period, emissions, target_scopes)
    
    ff_summary <- ff %>%
      group_by(OPEN_PROM_region, OPEN_PROM_sector_emi, period) %>%
      summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop")
    
    x <- left_join(ff_summary, NDC_Report, , by = c("OPEN_PROM_region", "OPEN_PROM_sector_emi", "period"))
    
    x <- x %>%
      group_by(OPEN_PROM_region, OPEN_PROM_sector_emi) %>%
      mutate(
        flag = {
          val_2025 <- emissions.x[period == 2025]
          ref_2025 <- emissions.y[period == 2025]
          any(val_2025 > ref_2025 * 1e6, na.rm = TRUE)
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
        NDC_Cit = {
          n <- length(amb_path)
          NDC_Cit <- numeric(n)         # create NDC_Cit
          NDC_Cit[1] <- amb_path[1]     # first value
          
          for(i in 2:n) {
            # compute increment only if previous and required values exist
            if(!is.na(NDC_Cit[i-1]) && !is.na(emissions.y[i]) && !is.na(emissions.y[i-1]) &&
               !is.na(emissions.y[1]) && !is.na(NDC_Cit[1])) {
              NDC_Cit[i] <- NDC_Cit[i-1] + (emissions.y[i] - emissions.y[i-1]) * NDC_Cit[1] / emissions.y[1]
            } else {
              NDC_Cit[i] <- NA  # propagate NA if any needed value is NA
            }
          }
          NDC_Cit
        }
      ) %>%
      ungroup()
    
    names(result) <- gsub("emissions.y", "NDC", names(result))
    
    result <- result %>%
      group_by(OPEN_PROM_region, OPEN_PROM_sector_emi) %>%
      arrange(period) %>%
      mutate(rate = (NDC - lag(NDC)) / lag(NDC)) %>%
      mutate(
        inst = ifelse(period == 2025, NDC - amb_path / 1e6, NA),
      ) %>%
      
      mutate(inst = first(inst[period == 2025]))  %>%
      mutate(
        rate = ifelse(period == 2025, 0, rate),
        NDC_Cit = cumprod(1+rate) * inst) %>%
      mutate(
        NDC_CC = NDC_Cit + amb_path / 1e6,
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
    
    all_var <- `OPEN-PROM_EmiSBS_Cities_OPEN-PROM_EmiSBS`
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
                          period %in% c(2025:2050)
    ) %>%
      select(OPEN_PROM_region, period, NDC, OPEN_PROM_sector_emi) %>%
      rename(emissions = NDC)
    
    restSBSEmi <- filter(restSBSEmi, OPEN_PROM_region %in% unique(NDC_sum_sub[["OPEN_PROM_region"]]))
    
    NDC_sum_sub <- rbind(NDC_sum_sub, restSBSEmi) %>% rename(value = emissions, region = OPEN_PROM_region, variable = OPEN_PROM_sector_emi)
    
    NDC_sum_sub <- as.quitte(NDC_sum_sub)
    
    NDC_CC_sum_sub <- filter(result,
                             period %in% c(2025:2050)
    ) %>%
      select(OPEN_PROM_region, period, NDC_CC, OPEN_PROM_sector_emi) %>%
      rename(emissions = NDC_CC)
    
    NDC_CC_sum_sub <- rbind(NDC_CC_sum_sub, restSBSEmi) %>% rename(value = emissions, region = OPEN_PROM_region, variable = OPEN_PROM_sector_emi)
    
    NDC_CC_sum_sub <- as.quitte(NDC_CC_sum_sub)
    
    
    SCC_sum_sub <- filter(result,
                          period %in% c(2025:2050)
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
    EU28 <- toolGetMapping(name = "EU28.csv",
                           type = "regional",
                           where = "mrprom")
    EU27 <- setdiff(EU28[["ISO3.Code"]],"GBR")
    
    ######   add extra countries
    extraCountries <- NDC_Report
    names(extraCountries) <- c("region","variable","period","value")
    extraCountries <- as.quitte(extraCountries) %>% as.magpie()
    countries <- setdiff(getRegions(extraCountries),getRegions(NDC_sum_sub))
    extraCountries <- extraCountries[countries,,intersect(getItems(NDC_sum_sub,3),getItems(extraCountries,3))]
    
    NDC_sum_sub <- NDC_sum_sub[,,intersect(getItems(NDC_sum_sub,3),getItems(extraCountries,3))]
    NDC_sum_sub <- mbind(NDC_sum_sub, extraCountries)
    NDC_CC_sum_sub <- NDC_CC_sum_sub[,,intersect(getItems(NDC_sum_sub,3),getItems(extraCountries,3))]
    NDC_CC_sum_sub <- mbind(NDC_CC_sum_sub, extraCountries)
    SCC_sum_sub <- SCC_sum_sub[,,intersect(getItems(NDC_sum_sub,3),getItems(extraCountries,3))]
    SCC_sum_sub <- mbind(SCC_sum_sub, extraCountries)
    ####################
    
    NDC_sum_sub_GLO <- dimSums(NDC_sum_sub, 1, na.rm = TRUE)
    getItems(NDC_sum_sub_GLO, 1) <- "World"
    NDC_sum_sub_EU27 <- dimSums(NDC_sum_sub[EU27,,], 1, na.rm = TRUE)
    getItems(NDC_sum_sub_EU27, 1) <- "EU"
    NDC_sum_sub2 <- mbind(NDC_sum_sub, NDC_sum_sub_GLO, NDC_sum_sub_EU27)
    NDC_sum_sub2[,,"Emissions|CO2|Energy|Supply"] <- NDC_sum_sub2[,,"Emissions|CO2|Energy|Supply"] +
      NDC_sum_sub2[,,"Gross Emissions|CO2|Energy|Supply|Electricity"]
    NDC_sum_sub2[,,"Emissions|CO2"] <- NDC_sum_sub2[,,"Emissions|CO2"] +
      NDC_sum_sub2[,,"Gross Emissions|CO2|Energy|Supply|Electricity"]
    
    NDC_sum_sub2ResNCom <- NDC_sum_sub2[, , c("Emissions|CO2|Energy|Demand|Residential","Emissions|CO2|Energy|Demand|Commercial")]
    NDC_sum_sub2ResNCom <- dimSums(NDC_sum_sub2ResNCom, dim = 3, na.rm = TRUE)
    getItems(NDC_sum_sub2ResNCom, 3) <- "Emissions|CO2|Energy|Demand|Residential and Commercial"
    NDC_sum_sub2 <- mbind(NDC_sum_sub2, NDC_sum_sub2ResNCom)
    
    NDC_CC_sum_sub_EU27 <- dimSums(NDC_CC_sum_sub[EU27,,], 1, na.rm = TRUE)
    getItems(NDC_CC_sum_sub_EU27, 1) <- "EU"
    NDC_CC_sum_sub_GLO <- dimSums(NDC_CC_sum_sub, 1, na.rm = TRUE)
    getItems(NDC_CC_sum_sub_GLO, 1) <- "World"
    NDC_CC_sum_sub2 <- mbind(NDC_CC_sum_sub, NDC_CC_sum_sub_GLO, NDC_CC_sum_sub_EU27)
    NDC_CC_sum_sub2[,,"Emissions|CO2|Energy|Supply"] <- NDC_CC_sum_sub2[,,"Emissions|CO2|Energy|Supply"] +
      NDC_CC_sum_sub2[,,"Gross Emissions|CO2|Energy|Supply|Electricity"]
    NDC_CC_sum_sub2[,,"Emissions|CO2"] <- NDC_CC_sum_sub2[,,"Emissions|CO2"] +
      NDC_CC_sum_sub2[,,"Gross Emissions|CO2|Energy|Supply|Electricity"]
    
    NDC_CC_sum_sub2ResNCom <- NDC_CC_sum_sub2[, , c("Emissions|CO2|Energy|Demand|Residential","Emissions|CO2|Energy|Demand|Commercial")]
    NDC_CC_sum_sub2ResNCom <- dimSums(NDC_CC_sum_sub2ResNCom, dim = 3, na.rm = TRUE)
    getItems(NDC_CC_sum_sub2ResNCom, 3) <- "Emissions|CO2|Energy|Demand|Residential and Commercial"
    NDC_CC_sum_sub2 <- mbind(NDC_CC_sum_sub2, NDC_CC_sum_sub2ResNCom)
    
    SCC_sum_sub_EU27 <- dimSums(SCC_sum_sub[EU27,,], 1, na.rm = TRUE)
    getItems(SCC_sum_sub_EU27, 1) <- "EU"
    SCC_sum_sub_GLO <- dimSums(SCC_sum_sub, 1, na.rm = TRUE)
    getItems(SCC_sum_sub_GLO, 1) <- "World"
    SCC_sum_sub2 <- mbind(SCC_sum_sub, SCC_sum_sub_GLO, SCC_sum_sub_EU27)
    SCC_sum_sub2[,,"Emissions|CO2|Energy|Supply"] <- SCC_sum_sub2[,,"Emissions|CO2|Energy|Supply"] +
      SCC_sum_sub2[,,"Gross Emissions|CO2|Energy|Supply|Electricity"]
    SCC_sum_sub2[,,"Emissions|CO2"] <- SCC_sum_sub2[,,"Emissions|CO2"] +
      SCC_sum_sub2[,,"Gross Emissions|CO2|Energy|Supply|Electricity"]
    
    SCC_sum_sub2ResNCom <- SCC_sum_sub2[, , c("Emissions|CO2|Energy|Demand|Residential","Emissions|CO2|Energy|Demand|Commercial")]
    SCC_sum_sub2ResNCom <- dimSums(SCC_sum_sub2ResNCom, dim = 3, na.rm = TRUE)
    getItems(SCC_sum_sub2ResNCom, 3) <- "Emissions|CO2|Energy|Demand|Residential and Commercial"
    SCC_sum_sub2 <- mbind(SCC_sum_sub2, SCC_sum_sub2ResNCom)
    
    write.report(NDC_sum_sub2, file = "all_cities.mif",model = "OPEN-PROM",scenario = "NDC",append = FALSE,unit = "MtCO2/yr")
    write.report(NDC_CC_sum_sub2, file = "all_cities.mif",model = "OPEN-PROM",scenario = "NDC_CC",append = TRUE,unit = "MtCO2/yr")
    write.report(SCC_sum_sub2, file = "all_cities.mif",model = "OPEN-PROM",scenario = "SCC",append = TRUE,unit = "MtCO2/yr")
    ##############
    # Step 1: Select columns from r2z_CP
    percent_ambition <- r2z_CP %>%
      select(
        climactor_id,           # 1
        target_scopes,               # 6
        T1_target_year,        # 13
        T2_target_year,        # 16
        T3_target_year,        # 19
        T4_target_year,        # 22
        T5_target_year, #25
        period,               
        value,                
        OPEN_PROM_region,     
        OPEN_PROM_sector_emi  
      )
    
    take_from_x <- x[,c(1,2,3,5)] %>%
      rename(value = emissions.y)
    
    # Step 2: Merge emissions.y from x based on region and sector
    percent_ambition <- percent_ambition %>%
      left_join(
        take_from_x,
        by = c("OPEN_PROM_region", "OPEN_PROM_sector_emi", "period")
      )
    
    percent_ambition <- percent_ambition %>%
      arrange(climactor_id, target_scopes, OPEN_PROM_region, OPEN_PROM_sector_emi, period) %>%
      group_by(climactor_id, target_scopes, OPEN_PROM_region, OPEN_PROM_sector_emi) %>%
      mutate(
        growth = (value.y - lag(value.y)) / lag(value.y),
        CompOP = {
          n <- n()
          out <- numeric(n)
          
          out[1] <- first(value.x)  # first value
          
          for (i in 2:n) {
            out[i] <- out[i-1] * (growth[i] + 1)
          }
          
          out
        }
      ) %>%
      ungroup()
    
    out_result <- percent_ambition %>%
      # Keep rows where any target year matches period
      filter(
        T1_target_year == period |
          T2_target_year == period |
          T3_target_year == period |
          T4_target_year == period |
          T5_target_year == period
      ) %>%
      # Group by climactor_id, OPEN_PROM_region, target_scopes, OPEN_PROM_sector_emi
      group_by(climactor_id, OPEN_PROM_region, target_scopes, OPEN_PROM_sector_emi) %>%
      # Count rows where value.x < CompOP
      summarise(rows_value_lt_emissions = sum(value.x < CompOP, na.rm = TRUE),
                total_rows = n(),
                .groups = "drop")
    
    total_sums <- out_result %>%
      summarise(
        sum_rows_value_lt_emissions = sum(rows_value_lt_emissions, na.rm = TRUE),
        sum_total_rows = sum(total_rows, na.rm = TRUE)
      )
    
    write.csv(total_sums, "All_Reg_Cit_Amb_percent.csv", row.names = TRUE)
    ###########################################
    All_Reg_Cit_Amb_percent <- result %>%
      select(OPEN_PROM_region, OPEN_PROM_sector_emi, period, NDC, NDC_CC, SCC)
    
    NDC_wide <- result %>%
      select(OPEN_PROM_region, period, NDC, OPEN_PROM_sector_emi)  %>% 
      distinct() %>%
      pivot_wider(
        names_from = period,   # each unique value of 'period' becomes a new column
        values_from = NDC  # fill the new columns with values from 'NDC_sum'
      )
    
    NDC_CC_wide <- result %>%
      select(OPEN_PROM_region, period, NDC_CC, OPEN_PROM_sector_emi) %>% 
      distinct()%>%
      pivot_wider(
        names_from = period,   # each unique value of 'period' becomes a new column
        values_from = NDC_CC  # fill the new columns with values from 'NDC_sum'
      )
    
    SCC_wide <- result %>%
      select(OPEN_PROM_region, period, SCC, OPEN_PROM_sector_emi) %>% 
      distinct()%>%
      pivot_wider(
        names_from = period,   # each unique value of 'period' becomes a new column
        values_from = SCC  # fill the new columns with values from 'NDC_sum'
      )
    
    library(openxlsx)
    df_list <- list(All_Reg_Cit_Amb_percent = All_Reg_Cit_Amb_percent,
                    NDC_wide = NDC_wide,
                    NDC_CC_wide = NDC_CC_wide,
                    SCC_wide = SCC_wide)  # replace with your actual 4th df
    wb <- createWorkbook()
    for (df_name in names(df_list)) {
      addWorksheet(wb, sheetName = df_name)
      writeData(wb, sheet = df_name, df_list[[df_name]])
    }
    
    saveWorkbook(wb, file = "ScenariosCities.xlsx", overwrite = TRUE)
    #####################################
    # 
    # final_result <- filter(result,
    #                        period %in% c(2025:2050)
    # ) %>%
    #   select(OPEN_PROM_region, period, NDC_sum, SCC_sum, NDC_CC_sum) %>% 
    #   distinct()
    # 
    # result_joined <- final_result %>%
    #   left_join(
    #     restSBSEmi_sum,
    #     by = c("OPEN_PROM_region", "period")
    #   )
    # 
    # result_joined_final <- result_joined %>%
    #   mutate(
    #     NDC_sum    = NDC_sum + emissions,
    #     SCC_sum    = SCC_sum + emissions,
    #     NDC_CC_sum = NDC_CC_sum + emissions
    #   )
    # 
    # # write.csv(result, "result.csv", row.names = TRUE)
    # 
    # 
    # plots <- result_joined_final
    # 
    # p <- plots %>%
    #   filter(
    #     period %in% 2024:2050
    #   ) %>%
    #   select(OPEN_PROM_region, period, NDC_sum, SCC_sum, NDC_CC_sum) %>%
    #   distinct() %>%
    #   rename(NDC = NDC_sum, NDC_CC = NDC_CC_sum, SCC = SCC_sum)  %>%
    #   pivot_longer(
    #     cols = c(NDC, NDC_CC, SCC),
    #     names_to = "variable",
    #     values_to = "value"
    #   ) %>%
    #   ggplot(aes(
    #     x = period,
    #     y = value,
    #     color = variable,
    #     group = variable
    #   )) +
    #   geom_line(size = 1) +
    #   geom_point(size = 1.5) +
    #   facet_wrap(~ OPEN_PROM_region, scales = "free_y") +  # <- key line
    #   labs(
    #     title = "All Regions",
    #     x = "Year",
    #     y = "Value",
    #     color = "Indicator"
    #   ) +
    #   theme_minimal()
    # 
    # # Save the plot
    # ggsave("all.png", plot = p, width = 12, height = 8, dpi = 300)
    # 
    # 
    
    
    #####################  END
  }
  # Define dimensions
  regions <- c("DEU", "FRA")
  years   <- c("2020", "2030", "2040")
  variables <- c("Dummy", "Dummy")
  
  # Create dummy data array
  data <- array(
    runif(length(regions) * length(years) * length(variables), 0, 100),
    dim = c(length(regions), length(years), length(variables)),
    dimnames = list(regions, years, variables)
  )
  
  # Create magpie object
  x <- as.magpie(data)
  list(x = x,
       weight = NULL,
       description = c(category = "ACHIEVE",
                       type = "ACHIEVE",
                       filename = "ACHIEVE.csv",
                       `Indicative size (MB)` = 3000,
                       dimensions = "4D",
                       unit = "various",
                       Confidential = "E3M"))
  
}
