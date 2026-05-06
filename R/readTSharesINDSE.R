#' readTSharesINDSE
#'
#' The dataset contains targets shares for INDSE.
#'
#' @return The read-in targets shares data into a magpie object
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("TSharesINDSE")
#' }
#'
#' @importFrom dplyr filter select group_by recode mutate ungroup
#' @importFrom tidyr pivot_longer pivot_wider complete
#' @importFrom readxl read_excel
#' @importFrom quitte as.quitte interpolate_missing_periods
#'
readTSharesINDSE <- function(subtype) {
  
  if (subtype == "PrimesProjections") {
    
    x <- readSource("PrimesBalancesGR", subtype = "Non")
    x <- as.quitte(x)
    
    x$value[x$value < 0.000001] <- 0
    
    x<-x[,-1]
    x<-filter(x, fuel == "Total")
    
    x_growth <- x %>% filter(period != 1995) %>% 
      arrange(scenario, region, variable, fuel, unit, period) %>%
      group_by(scenario, region, variable, fuel, unit) %>%
      mutate(
        growth_rate = case_when(
          value == 0 & lead(value) == 0 ~ 0,
          value == 0 ~ NA_real_,
          TRUE ~ lead(value) / value - 1
        ),
        growth_rate = lag(growth_rate)
      ) %>%
      ungroup()
    
    
    x_growth <- filter(x_growth, period > 2015) %>% select(-value) %>%
      rename(sector = variable, value = growth_rate)
    
    INDSE <- toolGetMapping(paste0("INDSE.csv"),
                            type = "blabla_export",
                            where = "mrprom")
    
    INDSE <- c(INDSE[,1],c("NEN","PCH"))
    
    x_growth <- x_growth %>%
      mutate(value = if_else(is.na(value), 0, value))#for one year of MALTA
    
    df <- x_growth %>% rename(Region = region, Sector = sector,Region = region, Sector = sector) %>%
      select(Region, Sector, period, value) %>%
      mutate(period = as.numeric(period)) %>%
      arrange(Region, Sector, period)
    
    # ------------------------------------------------------------
    # 5. Define blocks (endpoints)
    # ------------------------------------------------------------
    blocks <- df %>%
      group_by(Region, Sector) %>%
      arrange(period) %>%
      mutate(
        start_year = period,
        end_year   = lead(period),
        end_value  = lead(value)
      ) %>%
      filter(!is.na(end_year)) %>%
      select(Region, Sector,
             start_year, end_year, end_value)
    
    # ------------------------------------------------------------
    # 6. Full yearly grid (IMPORTANT FIX: ensure 2100 included)
    # ------------------------------------------------------------
    grid <- df %>%
      group_by(Region, Sector) %>%
      summarise(
        period = list(seq(min(period), max(period))),  # 2100 already in df
        .groups = "drop"
      ) %>%
      unnest(period)
    
    
    df_filled <- grid %>%
      left_join(df,
                by = c("Region", "Sector", "period")) %>%
      left_join(blocks,
                by = c("Region", "Sector")) %>%
      
      filter(period >= start_year & period <= end_year) %>%
      
      group_by(Region, Sector, period) %>%
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
    
    df_filled <- df_filled %>%
      select(region, period, sector, value) %>%
      rename(variable = sector) 
    
    x <- as.quitte(df_filled)
    x <- as.magpie(x)
    
    x <- add_columns(x, addnm = "OE", dim = 3, fill = NA)
    x[,,"OE"] <- x[,,"OI"]
    x <- x[,,INDSE]
    
    
    IFuelCons2 <- calcOutput(type = "IFuelCons2", aggregate = TRUE, regionmapping = "regionmappingOPDEV5.csv")
    
    IFuelCons2 <- IFuelCons2[,,INDSE][getRegions(x),,]
    IFuelCons2 <- dimSums(IFuelCons2, 3.2)
    
    x <- as.quitte(x)
    IFuelCons2 <- as.quitte(IFuelCons2) %>%
      select(-c("variable")) %>% rename(variable = "dsbs") %>%
      interpolate_missing_periods(period = seq(2010, 2070, 1), expand.values = TRUE)
    
    y <- as.quitte(x) %>%
      group_by(region, variable) %>%
      mutate(value = cumprod(1 +value)) %>% ungroup()
    
    combinedf <- full_join(y, IFuelCons2, by = c("region", "period", "variable", "model", "scenario", "unit")) %>%
      mutate(value = ifelse(period <= 2023, value.y, value.x * value.y))  %>%
      select(region, period, variable, value) %>% filter(period >= 2024)
    
    x <- as.quitte(combinedf)
    x <- as.magpie(x)
  }
  
  if (subtype == "PrimesShares") {
    
    x <- readSource("PrimesBalancesGR", subtype = "Non")
    
    INDSE <- toolGetMapping(paste0("INDSE.csv"),
                            type = "blabla_export",
                            where = "mrprom")
    
    INDSE <- c(INDSE[,1],c("NEN","PCH"))
    
    x <- add_columns(x, addnm = "OE", dim = 3.2, fill = NA)
    x[,,"OE"] <- x[,,"OI"]
    x <- x[,,INDSE]
    
    x <- x[,,INDSE]
    
    x <- as.quitte(x)
    
    x$value[x$value < 0.000001] <- 0
    
    x<-x[,-1]
    
    totals <- x %>%
      filter(fuel == "Total") %>%
      select(region, variable, period, total_value = value)
    
    q_share <- x %>%
      filter(fuel != "Total") %>%
      left_join(totals, by = c("region", "variable", "period")) %>%
      mutate(
        share = ifelse(total_value == 0, 0, value / total_value)
      )
    
    x <- q_share %>%
      select(region, variable, period, fuel, share) %>%
      rename(value = share) %>%
      mutate(period = as.numeric(period))
    
    x <- as.quitte(x) %>% filter(period > 2022) %>%
      interpolate_missing_periods(period = 2023:2070, expand.values = TRUE) 
    
    x <- as.magpie(x)
    
    IFuelCons2 <- calcOutput(type = "IFuelCons2", aggregate = TRUE, regionmapping = "regionmappingOPDEV5.csv")
    
    IFuelCons2 <- IFuelCons2[,,INDSE]
    items <- getItems(IFuelCons2, 3.2)
    
    x <- add_columns(x, addnm = setdiff(items, getItems(x,3.2)), dim = "fuel", fill = NA)
    
  }
  
  list(x = x,
       weight = NULL,
       description = c(category = "targets shares for INDSE",
                       type = "targets shares for INDSE",
                       filename = "SharesOP.xlsx",
                       `Indicative size (MB)` = 0.13,
                       dimensions = "3D",
                       unit = "shares",
                       Confidential = "E3M"))
  
}