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
#' @importFrom dplyr %>%  select
#' @importFrom readxl read_excel excel_sheets
#' @importFrom tidyr pivot_longer
#'

readACHIEVE <- function() {
  
  # List all .xlsx files in the folder
  files <- list.files(pattern = "\\.xlsx$", full.names = TRUE)
  
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
  
  r2z_CP <- r2z_CP[c(1:10000),] %>%
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
  
  x <- left_join(r2z_CP, growth_OP, , by = c("OPEN_PROM", "OPEN-PROM Subsector Emission Growth Rates", "period"))
  
  # Ensure your data is sorted correctly within each group
  x <- x %>%
    arrange(`OPEN_PROM`, `OPEN-PROM Subsector Emission Growth Rates`, company_id,period) %>%
    group_by(`OPEN_PROM`, `OPEN-PROM Subsector Emission Growth Rates`,company_id) %>%
    mutate(
      emissions = emissions.x  # start with existing values
    )
  
  # Loop over rows within each group
  x <- x %>%
    group_split() %>%  # split by group
    map_dfr(function(df) {
      for(i in 2:nrow(df)) {
        if(is.na(df$emissions[i])) {
          # use previous emissions * (1 + growth/100)
          df$emissions[i] <- df$emissions[i-1] * (1 + df$emissions.y[i] / 100)
        }
      }
      df
    })
  
  # Ungroup final dataframe
  x <- x %>% ungroup()
  
  return(x)
  
}
