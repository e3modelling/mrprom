#' readTSharesDOMSE
#'
#' The dataset contains targets shares for DOMSE.
#'
#' @return The read-in targets shares data into a magpie object
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("TSharesDOMSE")
#' }
#'
#' @importFrom dplyr filter select group_by ungroup mutate recode
#' @importFrom dplyr arrange case_when lead rename summarise
#' @importFrom dplyr left_join slice across
#' @importFrom tidyr pivot_longer pivot_wider complete unnest fill full_seq
#' @importFrom readxl read_excel
#' @importFrom quitte as.quitte interpolate_missing_periods
#'
readTSharesDOMSE <- function(subtype) {
  
  if (subtype == "Shares") {
    x <- read_excel("IEATargetsBuildings 2.xlsx", col_names = TRUE, sheet = "SharesHOU-SE")
    
    mapping_fuels <- c(
      "Total" = "Total",
      "Hard Coal, Coke and Other Solids" = "HCL",
      "Lignite" = "LGN",
      "Crude Oil and Feedstocks" = "CRO",
      "Liquefied Petroleum Gas" = "LPG",
      "Gasoline" = "GSL",
      "Kerosene" = "KRS",
      "Diesel Oil" = "GDO",
      "Residual Fuel Oil" = "RFO",
      "Other Liquids" = "OLQ",
      "Natural Gas" = "NGS",
      "Other Gases" = "OGS",
      "Nuclear" = "NUC",
      "Steam" = "STE",
      "Hydro" = "HYD",
      "Wind" = "WND",
      "Solar" = "SOL",
      "Biomass and Waste" = "BMSWAS",
      "Geothermal and other renewable sources" = "GEO",
      "Methanol" = "MET",
      "Ethanol" = "ETH",
      "Biodiesel" = "BGDO",
      "Biogasoline" = "BGSL",
      "Biokerosene" = "BKRS",
      "Hydrogen" = "H2F",
      "Electricity" = "ELC"
    )
    
    x <- x %>%
      mutate(Product = recode(Product, !!!mapping_fuels))
    
    x <- x %>%
      mutate(
        Sector = case_when(
          Sector == "Commercial"  ~ "SE",
          Sector == "Residential" ~ "HOU",
          TRUE ~ Sector
        )
      )
    
    x <- x %>% pivot_longer(!c("Region", "Sector", "Product"), names_to = "period", values_to = "value")
    
    x <- as.quitte(x) %>%
      interpolate_missing_periods(period = 2023:2100, expand.values = TRUE)
    
    # x <- x %>%
    #   select(region, period, sector, product, value) %>%
    #   rename(variable = sector, fuel = product) %>%
    #   pivot_wider(
    #     names_from = period,
    #     values_from = value
    #   )
    
    x <- x %>%
      select(region, period, sector, product, value) %>%
      rename(variable = sector, fuel = product) 
    
    x <- filter(x, fuel != "Total") %>% 
      filter(period > 2023)
    
    
    # library(writexl)
    # 
    # write_xlsx(x, "sharesIEA.xlsx")
    
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "Projections") {

    # ------------------------------------------------------------
    # 1. Read data
    # ------------------------------------------------------------
    x <- read_excel(
      "IEATargetsBuildings 2.xlsx",
      col_names = TRUE,
      sheet = "GrowthRatesTotal-HOU-SE"
    )
    
    # ------------------------------------------------------------
    # 2. Long format
    # ------------------------------------------------------------
    x <- x %>%
      pivot_longer(
        !c("Region", "Sector", "Product"),
        names_to = "period",
        values_to = "value"
      )
    
    # ------------------------------------------------------------
    # 3. Harmonise sector names
    # ------------------------------------------------------------
    x <- x %>%
      mutate(
        Sector = case_when(
          Sector == "Commercial"  ~ "SE",
          Sector == "Residential" ~ "HOU",
          TRUE ~ Sector
        )
      )
    
    # ------------------------------------------------------------
    # 4. Prepare df
    # ------------------------------------------------------------
    df <- x %>%
      mutate(period = as.numeric(period)) %>%
      arrange(Region, Sector, Product, period)
    
    # ------------------------------------------------------------
    # 5. Define blocks (endpoints)
    # ------------------------------------------------------------
    blocks <- df %>%
      group_by(Region, Sector, Product) %>%
      arrange(period) %>%
      mutate(
        start_year = period,
        end_year   = lead(period),
        end_value  = lead(value)
      ) %>%
      filter(!is.na(end_year)) %>%
      select(Region, Sector, Product,
             start_year, end_year, end_value)
    
    # ------------------------------------------------------------
    # 6. Full yearly grid (IMPORTANT FIX: ensure 2100 included)
    # ------------------------------------------------------------
    grid <- df %>%
      group_by(Region, Sector, Product) %>%
      summarise(
        period = list(seq(min(period), max(period))),  # 2100 already in df
        .groups = "drop"
      ) %>%
      unnest(period)
  
    
    df_filled <- grid %>%
      left_join(df,
                by = c("Region", "Sector", "Product", "period")) %>%
      left_join(blocks,
                by = c("Region", "Sector", "Product")) %>%
      
      filter(period >= start_year & period <= end_year) %>%
      
      group_by(Region, Sector, Product, period) %>%
      slice(1) %>%
      ungroup() %>%
      
      mutate(
        value = (1 + end_value)^(1 / (end_year - start_year)) - 1
      )
    # ------------------------------------------------------------
    # 8. Convert format
    # ------------------------------------------------------------
    df_filled <- as.quitte(df_filled) %>% 
       filter(period > 2023) 
    
    # df_filled <- df_filled %>%
    #   select(region, period, sector, value) %>%
    #   rename(variable = sector) %>%
    #   pivot_wider(
    #     names_from = period,
    #     values_from = value
    #   )
    
    df_filled <- df_filled %>%
      select(region, period, sector, value) %>%
      rename(variable = sector) 
 
    # library(writexl)
    # 
    # write_xlsx(df_filled, "growth_rates_IEA.xlsx")
    
    x <- as.quitte(df_filled)
    x <- as.magpie(x)
  }

  if (subtype == "growthPrimesBalancesStepWiseExtension") {
    
    # ------------------------------------------------------------
    # 1. Read data
    # ------------------------------------------------------------
  
    x <- read_excel(
      "growthPrimesBalancesStepWiseExtension.xlsx",
      col_names = TRUE,
      sheet = "Sheet1"
    )
    
    x <- x %>%
      mutate(across(!c(region, variable), as.numeric)) %>%
      pivot_longer(
        !c(region, variable),
        names_to = "period",
        values_to = "value"
      ) %>%
      filter(variable %in% c("AG", "SE", "HOU")) %>%
      mutate(period = as.numeric(period)) %>%
      
      group_by(region, variable) %>%
      arrange(period) %>%
      mutate(value = value^(1/5) - 1) %>%
      # create missing years if needed
      complete(period = full_seq(period, 1)) %>%
      
      # fill NAs with NEXT value
      fill(value, .direction = "up") %>%
      
      ungroup() %>% filter(period > 2023)
    
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "PrimesSharesExtension") {
    
    # ------------------------------------------------------------
    # 1. Read data
    # ------------------------------------------------------------
    
    x <- read_excel(
      "PrimesSharesExtension.xlsx",
      col_names = TRUE,
      sheet = "Sheet1", col_types = "text"
    )
    
    x <- x %>%
      mutate(across(!c(region, variable, fuel), as.numeric)) %>%
      pivot_longer(
        !c(region, variable, fuel),
        names_to = "period",
        values_to = "value"
      ) %>%
      filter(variable %in% c("AG", "SE", "HOU")) %>%
      mutate(period = as.numeric(period)) %>% filter(period > 2023)
             
    x <- as.quitte(x) %>%
      interpolate_missing_periods(period = 2023:2100, expand.values = TRUE)
    
    x <- as.magpie(x)
    
  }
  
  list(x = x,
       weight = NULL,
       description = c(category = "targets shares for DOMSE",
                       type = "targets shares for DOMSE",
                       filename = "SharesOP.xlsx",
                       `Indicative size (MB)` = 0.13,
                       dimensions = "3D",
                       unit = "shares",
                       Confidential = "E3M"))
  
}