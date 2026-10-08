#' readIEA_WEO_2025_ExtendedData
#'
#' Read in the WEO 2025 Extended Data Regions file for extracting industry production. More variables are included
#' 
#' @param subtype The folder name containing the data file.
#' 
#' @return The read-in data into a magpie object.
#'
#' @author Sonja Sechi
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_WEO_2025_ExtendedData", subtype = "IEA_WEO_2025_ExtendedData")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr select rename mutate
#' @importFrom tidyr drop_na
#' @importFrom quitte as.quitte

readIEA_WEO_2025_ExtendedData <- function(subtype = "IEA_WEO_2025_ExtendedData") {
  
  if (subtype == "IEA_WEO_2025_ExtendedData") {
    
    # Read the CSV file
    data <- read.csv("WEO2025_Extended_Data_Regions.csv")
    
    # Select only the relevant columns
    data <- data %>% select(REGION, YEAR, SCENARIO, FLOW, PRODUCT, UNIT, VALUE, CATEGORY)
    
    # Rename columns for consistency
    data <- data %>% rename(
      region = REGION,
      period = YEAR,
      scenario = SCENARIO,
      variable = FLOW,
      product = PRODUCT,
      unit = UNIT,
      value = VALUE,
      category = CATEGORY
    )
    # Assign "WEO 2025" to a new column "model"
    data <- data %>% mutate(model = "WEO 2025")
    
    # Convert numeric values to appropriate formats
    data <- data %>% mutate(
      value = as.numeric(value),
      period = as.numeric(period)
    )
    
    # Remove rows with missing values
    data <- data %>% drop_na()
    
    # Convert data into a magpie object
    data <- as.quitte(data) %>% as.magpie()
    
  }
  
  if (subtype == "Prices") {
    
    # Read Excel sheet
    df <- read_excel(
      "WEO2025_Extended_Data.xlsx",
      sheet = "Prices"
    )
    
    # Keep only the relevant columns and rows
    df <- df[, -c(1:9, 13)]
    df <- df[-c(1:7), ]
    
    # Store scenario names from the original Excel column headers
    scenario <- case_when(
      str_detect(names(df), "^Historical") ~
        "Historical",
      
      str_detect(names(df), "^Current Policies Scenario") ~
        "Current Policies Scenario",
      
      str_detect(names(df), "^Stated Policies Scenario") ~
        "Stated Policies Scenario",
      
      str_detect(names(df), "^Net Zero Emissions by 2050 Scenario") ~
        "Net Zero Emissions by 2050 Scenario",
      
      TRUE ~ NA_character_
    )
    
    # First remaining row contains the years
    year <- as.character(
      unlist(df[1, ], use.names = FALSE)
    )
    
    # First column contains names
    scenario[1] <- NA_character_
    year[1] <- "name"
    
    # Remove empty separator columns
    keep <- seq_along(year) == 1 |
      (
        !is.na(scenario) &
          !is.na(year) &
          year != ""
      )
    
    df <- df[, keep]
    scenario <- scenario[keep]
    year <- year[keep]
    
    # Create unique column names using scenario and year
    names(df) <- c(
      "name",
      paste(
        scenario[-1],
        year[-1],
        sep = "___"
      )
    )
    
    # Remove the row that contained years
    df <- df[-1, ]
    
    # Full fuel names exactly as shown in Excel
    fuel_names <- c(
      "IEA crude oil ($/barrel)",
      "Natural gas ($/MBtu)",
      "Steam coal ($/tonne)"
    )
    
    # Create fuel and region columns
    df <- df %>%
      mutate(
        fuel = case_when(
          name %in% fuel_names ~ name,
          TRUE ~ NA_character_
        )
      ) %>%
      fill(fuel) %>%
      mutate(
        region = case_when(
          name == "IEA crude oil ($/barrel)" ~ "World",
          
          name %in% c(
            "Natural gas ($/MBtu)",
            "Steam coal ($/tonne)"
          ) ~ NA_character_,
          
          TRUE ~ name
        )
      ) %>%
      filter(!is.na(region))
    
    # Convert all price columns to numeric
    df <- df %>%
      mutate(
        across(
          -c(name, fuel, region),
          ~ suppressWarnings(as.numeric(.x))
        )
      )
    
    # Pivot to long format
    df_long <- df %>%
      pivot_longer(
        cols = -c(name, fuel, region),
        names_to = c("scenario", "year"),
        names_sep = "___",
        values_to = "value"
      ) %>%
      mutate(
        year = as.integer(year)
      ) %>%
      filter(!is.na(value)) %>%
      select(
        fuel,
        region,
        scenario,
        year,
        value
      ) %>%
      arrange(
        fuel,
        region,
        scenario,
        year
      ) 
    
    df_long <- df_long %>%
      mutate(
        region = if_else(
          is.na(fuel),
          "WORLD",
          region
        )
      )
    
    df_long <- df_long %>%
      mutate(
        fuel = if_else(
          is.na(fuel),
          "IEA crude oil ($/barrel)",
          fuel
        )
      )
    
    
    # Remove rows with missing values
    data <- df_long %>% drop_na()
    
    # Convert data into a magpie object
    data <- as.quitte(data) %>% as.magpie()
    
  }
  
  # Return the processed dataset as a list
  list(x = data,
       weight = NULL,
       description = c(category = "Energy",
                       type = "WEO 2025 Extended Data",
                       filename = "WEO2025_Extended_Data_Regions.csv",
                       `Indicative size (MB)` = 0.05,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "IEA"))
}
