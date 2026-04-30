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
#' @importFrom dplyr filter select group_by recode
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel
#' @importFrom quitte as.quitte
#'
readTSharesDOMSE <- function(subtype) {
  
  if (subtype == "SharesOP") {
    x <- read_excel("IEATargetsBuildings.xlsx", col_names = TRUE, sheet = "SharesHOU-SE")
    
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
    
    # x <- filter(x, !(`OP-Product` %in% c("OPEN-PROM Total", "Source Total")))
    
    x <- as.quitte(x) %>%
      interpolate_missing_periods(period = 2010:2100, expand.values = TRUE)
    
    x <- x %>%
      select(region, period, sector, product, value) %>%
      rename(variable = sector, fuel = product) %>%
      pivot_wider(
        names_from = period,
        values_from = value
      )
    
    x <- filter(x, fuel != "Total")
    
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "ProjectionsOP") {
    x <- read_excel("IEATargetsBuildings.xlsx", col_names = TRUE, sheet = "GrowthRatesTotal-HOU-SE")
    
    x <- x %>% pivot_longer(!c("Region", "Sector", "Product"), names_to = "period", values_to = "value")
    
    x <- x %>%
      mutate(
        Sector = case_when(
          Sector == "Commercial"  ~ "SE",
          Sector == "Residential" ~ "HOU",
          TRUE ~ Sector
        )
      )
    
    # x <- x %>%
    #   mutate(value = ifelse(period > 2050 & value == 0, NA, value))
    
    # x <- as.quitte(x) %>%
      # interpolate_missing_periods(period = 2023:2100, expand.values = FALSE)
    # df <- x %>%
    #   mutate(period = as.numeric(period)) %>%
    #   arrange(Region, Sector, Product, period)
    # 
    # # 1. Define blocks (ONLY endpoints, no growth calculation stored)
    # blocks <- df %>%
    #   group_by(Region, Sector, Product) %>%
    #   arrange(period) %>%
    #   mutate(
    #     start_year = period,
    #     end_year   = lead(period),
    #     end_value  = lead(value)
    #   ) %>%
    #   filter(!is.na(end_year)) %>%
    #   select(Region, Sector, Product, start_year, end_year, end_value)
    # 
    # # 2. Full yearly grid
    # grid <- df %>%
    #   group_by(Region, Sector, Product) %>%
    #   summarise(
    #     period = list(seq(min(period), max(period))),
    #     .groups = "drop"
    #   ) %>%
    #   unnest(period)
    # 
    # # 3. Assign each year to correct block + apply formula directly
    # df_filled <- grid %>%
    #   left_join(df, by = c("Region","Sector","Product","period")) %>%
    #   left_join(blocks, by = c("Region","Sector","Product")) %>%
    #   filter(period >= start_year & period < end_year) %>%
    #   group_by(Region, Sector, Product, period) %>%
    #   slice(1) %>%
    #   ungroup() %>%
    #   mutate(
    #     value = ifelse(
    #       is.na(value),
    #       (1 + end_value)^(1 / (end_year - start_year)) - 1,
    #       value
    #     )
    #   )
    
    
    df <- x %>%
      mutate(period = as.numeric(period)) %>%
      arrange(Region, Sector, Product, period)
    
    # 1. Define blocks (endpoints only)
    blocks <- df %>%
      group_by(Region, Sector, Product) %>%
      arrange(period) %>%
      mutate(
        start_year = period,
        end_year   = lead(period),
        end_value  = lead(value)
      ) %>%
      filter(!is.na(end_year)) %>%
      select(Region, Sector, Product, start_year, end_year, end_value)
    
    # 2. Full yearly grid
    grid <- df %>%
      group_by(Region, Sector, Product) %>%
      summarise(
        period = list(seq(min(period), max(period))),
        .groups = "drop"
      ) %>%
      unnest(period)
    
    # 3. Assign blocks and apply your corrected formula
    df_filled <- grid %>%
      left_join(df, by = c("Region", "Sector", "Product", "period")) %>%
      left_join(blocks, by = c("Region", "Sector", "Product")) %>%
      filter(period >= start_year & period < end_year) %>%
      group_by(Region, Sector, Product, period) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(
        value = ifelse(
          is.na(value),
          (1 + end_value)^(1 / (end_year - (start_year + 1))) - 1,
          value
        )
      )
    
    df_filled <- as.quitte(df_filled)
    
    df_filled <- df_filled %>%
      select(region, period, sector, value) %>%
      rename(variable = sector) %>%
      pivot_wider(
        names_from = period,
        values_from = value
      )
    
    library(openxlsx)
    
    write_xlsx(df_filled, "growth_rates_of_projections.xlsx")
    
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