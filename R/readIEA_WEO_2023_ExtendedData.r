#' readIEA_WEO_2023_ExtendedData
#'
#' Read in the WEO 2023 Extended Data Regions file for extracting industry production. More variables are included
#' 
#' @param subtype The folder name containing the data file.
#' 
#' @return The read-in data into a magpie object.
#'
#' @author Sonja Sechi
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_WEO_2023_ExtendedData", subtype = "IEA_WEO_2023_ExtendedData")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr select rename mutate
#' @importFrom tidyr drop_na
#' @importFrom quitte as.quitte

readIEA_WEO_2023_ExtendedData <- function(subtype = "IEA_WEO_2023_ExtendedData") {
  
  if (subtype == "IEA_WEO_2023_ExtendedData") {
    
    # Read the CSV file
    data <- read.csv("WEO2023_Extended_Data_Regions.csv")
    
    # Select only the relevant columns
    data <- data %>% select(REGION, YEAR, SCENARIO, FLOW, PRODUCT, UNIT, VALUE)
    
    # Rename columns for consistency
    data <- data %>% rename(
      region = REGION,
      period = YEAR,
      scenario = SCENARIO,
      variable = FLOW,
      product = PRODUCT,
      unit = UNIT,
      value = VALUE
    )
    
    # Convert numeric values to appropriate formats
    data <- data %>% mutate(
      value = as.numeric(value),
      period = as.numeric(period)
    )
    
    # Remove rows with missing values
    data <- data %>% drop_na()
    
    # Convert data into a magpie object
    data <- as.quitte(data) %>% as.magpie()
    
    # Return the processed dataset as a list
    list(x = data,
         weight = NULL,
         description = c(category = "Energy",
                         type = "WEO 2023 Extended Data",
                         filename = "WEO2023_Extended_Data_Regions.csv",
                         `Indicative size (MB)` = 0.05,
                         dimensions = "3D",
                         unit = "various",
                         Confidential = "IEA"))
  }
}
