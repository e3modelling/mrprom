#' readDieselGasTaxesEurope
#'
#' Read in data Diesel Gas Taxes Europe.
#' The dataset contains Diesel Gas Taxes Europe data from taxfoundation.org/data/all/eu/diesel-gas-taxes-europe/.
#'
#' @return The read-in carbon price data into a magpie object
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("DieselGasTaxesEurope")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter select mutate across na_if if_else
#' @importFrom tidyr pivot_longer
#' @importFrom readr parse_number
#' @importFrom quitte as.quitte
#'
readDieselGasTaxesEurope <- function() {
  
  x <- read.csv(file = "Diesel and Gas Taxes in Europe, 2026  Tax Foundation.csv")
  
  names(x) <- sub("X", "region", names(x))
  
    x[["region"]] <- toolCountry2isocode(x[["region"]], mapping = c("EU Average" = "EU Average",
                                                                    "EU Minimum Rate" = "EU Minimum Rate"))
    
    x <- x %>%
      # 1. Force all columns to be characters so pivot_longer can safely stack them
      mutate(across(-region, as.character)) %>%
      
      # 2. Reshape the data from wide to long layout
      pivot_longer(
        cols = -region,
        names_to = "variable",  # Changed from 'period' to match what it actually is
        values_to = "value"
      ) %>%
      
      # 3. Clean up the character characters, percentages, and currencies
      mutate(
        # Handle the "-" missing placeholders safely
        value = na_if(trimws(value), "-"),
        
        # Check for percentage strings, divide by 100 if true, otherwise parse normally
        value = if_else(
          grepl("%", value),
          parse_number(value) / 100,
          parse_number(value)
        )
      )
    
    
    x <- as.quitte(x) %>% as.magpie()
    getItems(x,2) <- "2026"
    
    Per_Gallon <- x[,,c("Gas_Tax_Per_Gallon_in_USD", "Diesel_Tax_Per_Gallon_in_USD", "Additional_VAT_Rate")]
    
    # Constants
    GJ_per_toe <- 41.868
    EUR_USD_2015 <- 1.1095
    inflation_factor <- 1.35  # Replace with verified 2026/2015 inflation factor
    
    # Fuel properties
    density <- c(GSL = 0.745, GDO = 0.840)
    NCV <- c(GSL = 44.3, GDO = 42.6)
    
    # Keep only EUR/litre variables
    x <- x[, , c(
      "Gas_Tax_Per_Liter_in_EUR",
      "Diesel_Tax_Per_Liter_in_EUR"
    )]
    
    # Convert EUR2026/litre to kUSD2015/toe
    x[, , 1] <- x[, , 1] / inflation_factor *
      EUR_USD_2015 * GJ_per_toe /
      (density["GSL"] * NCV["GSL"])
    
    x[, , 2] <- x[, , 2] / inflation_factor *
      EUR_USD_2015 * GJ_per_toe /
      (density["GDO"] * NCV["GDO"])
    
    # Rename variables with units
    getItems(x, 3) <- c(
      "Gas Tax Europe (kUSD2015/toe)",
      "Diesel Tax Europe (kUSD2015/toe)"
    )
    
    # Set dimension name
    getSets(x)[3] <- "variable"
  
  
  list(x = x,
       weight = NULL,
       description = c(category = "Diesel Gas Taxes Europe",
                       type = "Diesel Gas Taxes Europe",
                       filename = "Diesel and Gas Taxes in Europe, 2026  Tax Foundation.csv",
                       `Indicative size (MB)` = 0.03,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "project"))
  
}
