#' calcIDataAgriculturePrice
#'
#' @return  Magpie object with Agriculture Price
#'
#' @author Fotis Sioutas
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataAgriculturePrice", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter left_join mutate rename select case_when %>%
#' @importFrom eurostat get_eurostat
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIDataAgriculturePrice <- function() {
  
  fEndY <- toolReadEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndY"]
  fStartHorizon <- toolReadEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  

  fuelMapping <- c(
    "20210000" = "ELC",  # Electricity
    "20222000" = "RFO",  # Residual fuel oil
    "20231000" = "GSL",  # Motor spirit / gasoline
    "20232000" = "GDO"   # Automotive diesel oil
  )
  
  # ------------------------------------------------------------
  # Agricultural input prices
  # ------------------------------------------------------------
  
  dataAgriPrice <- get_eurostat(
    "apri_ap_ina",
    type = "code",
    time_format = "num",
    stringsAsFactors = FALSE
  ) %>%
    filter(
      freq == "A",
      currency == "EUR",
      prod_inp %in% names(fuelMapping)
    ) %>%
    mutate(
      prod_inp = fuelMapping[prod_inp]
    ) %>%
    rename(
      fuel   = prod_inp,
      region = geo,
      period = TIME_PERIOD,
      value  = values
    ) %>%
    select(
      fuel,
      region,
      period,
      value
    )
  
  
  # ------------------------------------------------------------
  # HICP deflator
  # ------------------------------------------------------------
  
  deflator <- get_eurostat(
    "prc_hicp_aind",
    type = "code",
    time_format = "num",
    stringsAsFactors = FALSE
  ) %>%
    filter(
      coicop == "CP00",
      unit == "INX_A_AVG"
    ) %>%
    rename(
      region = geo,
      period = TIME_PERIOD,
      hicp   = values
    ) %>%
    select(
      region,
      period,
      hicp
    ) %>%
    mutate(
      period = as.integer(period)
    )
  
  
  # ------------------------------------------------------------
  # Rebase HICP to 2015 = 100
  # ------------------------------------------------------------
  
  hicp2015 <- deflator %>%
    filter(period == 2015) %>%
    select(
      region,
      hicp2015 = hicp
    )
  
  deflator <- deflator %>%
    left_join(
      hicp2015,
      by = "region"
    ) %>%
    mutate(
      deflator = hicp / hicp2015 * 100
    ) %>%
    select(
      region,
      period,
      deflator
    )
  
  
  # ------------------------------------------------------------
  # Physical conversion constants
  # ------------------------------------------------------------
  
  # 1 toe = 41.868 GJ
  GJ_per_toe <- 41.868
  
  # Density [kg/litre]
  density <- c(
    "GDO" = 0.84,
    "RFO" = 0.99,
    "GSL" = 0.745
  )
  
  # Net calorific value [GJ/tonne]
  NCV <- c(
    "GDO" = 42.6,
    "RFO" = 40.0,
    "GSL" = 44.3
  )
  
  # 2015 average exchange rate
  # 1 EUR = 1.1095 USD
  EUR_USD_2015 <- 1.1095
  
  
  # ------------------------------------------------------------
  # Convert to kUSD2015/toe
  # ------------------------------------------------------------
  
  dataAgriPrice <- dataAgriPrice %>%
    mutate(
      period = as.integer(period),
      
      quantity_toe = case_when(
        
        # Electricity: EUR / 1000 kWh
        # 1000 kWh = 3.6 GJ
        fuel == "ELC" ~
          0.36 / GJ_per_toe,
        
        # Liquid fuels: EUR / 100 litres
        fuel %in% c("GDO", "RFO", "GSL") ~
          (
            100 *
              density[fuel] /
              1000 *
              NCV[fuel]
          ) / GJ_per_toe,
        
        TRUE ~ NA_real_
      ),
      
      # EUR / original quantity -> EUR / toe
      EUR_per_toe = value / quantity_toe
    ) %>%
    
    left_join(
      deflator,
      by = c("region", "period")
    ) %>%
    
    mutate(
      # Nominal EUR -> constant 2015 EUR
      EUR2015_per_toe =
        EUR_per_toe * 100 / deflator,
      
      # EUR2015/toe -> USD2015/toe
      USD2015_per_toe =
        EUR2015_per_toe * EUR_USD_2015,
      
      # USD2015/toe -> kUSD2015/toe
      value =
        USD2015_per_toe / 1000
    ) %>%
    
    select(
      region,
      period,
      fuel,
      value
    ) 
  
  suppressWarnings({
    dataAgriPrice[["region"]] <- toolCountry2isocode(dataAgriPrice[["region"]],
                                          mapping =
                                            c(
                                              "EU28" = "EU28",
                                              "EU27" = "EU27",
                                              "EU12" = "EU12",
                                              "EU15" = "EU15",
                                              "EU27noUK" = "EU27noUK",
                                              "EL" = "GRC"
                                            )
    )
  })
  
  dataAgriPrice <- filter(dataAgriPrice, !is.na(dataAgriPrice[["region"]]))
  dataAgriPrice <- as.quitte(dataAgriPrice)
  dataAgriPrice <- as.magpie(dataAgriPrice)
  
  dataAgriPrice <- add_dimension(dataAgriPrice, dim = 3.2, add = "unit", nm =  "kUSD2015/toe")
  dataAgriPrice <- add_dimension(dataAgriPrice, dim = 3.3, add = "variable", nm =  "AG")
  
  SharesFuelPrices <- calcOutput("SharesFuelPrices", aggregate = FALSE)
  SharesFuelPrices <- SharesFuelPrices[,2025,]
  
  MultByShare <- dataAgriPrice
  
  MultByShare <- add_columns(MultByShare, addnm = "BGDO", dim = 3.1, fill = NA)
  MultByShare <- add_columns(MultByShare, addnm = "BGSL", dim = 3.1, fill = NA)
  
  # BGDO
  MultByShare[,,"BGDO.kUSD2015/toe.AG"] <- MultByShare[,,"GDO.kUSD2015/toe.AG"] * (SharesFuelPrices[getRegions(MultByShare),,"PC.shareBGDO"])

  # BGSL
  MultByShare[,,"BGSL.kUSD2015/toe.AG"] <- MultByShare[,,"GSL.kUSD2015/toe.AG"] * (SharesFuelPrices[getRegions(MultByShare),,"PC.shareBGSL"])
  
  dataAgriPrice <- MultByShare
  
  a <- calcOutput(type = "PrimaryEnergyPrice", aggregate = FALSE)
  BMSWAS <- a[,,"BMSWAS.kUSD2015/toe"]
  BMSWAS <- add_dimension(BMSWAS, dim = 3.3, add = "variable", nm =  "AG")
  names(dimnames(BMSWAS))[3] <-  "fuel.unit.variable"
  
  IFuelPrice <- calcOutput(type = "IFuelPrice", aggregate = FALSE)
  names(dimnames(IFuelPrice))[3] <-  "variable.unit.fuel"
  AGFuelPrice <- IFuelPrice[,,"AG"]
  
  # BGAS
  AGFuelPrice[,,"AG.USD2015/toe.BGAS"] <- AGFuelPrice[,,"AG.USD2015/toe.NGS"] * (SharesFuelPrices[,,"PC.shareBGAS"])
  
  AGFuelPrice <- AGFuelPrice[,fStartHorizon : fEndY,]
  BMSWAS <- BMSWAS[,fStartHorizon : fEndY,]
  dataAgriPrice <- dataAgriPrice[,fStartHorizon : fEndY,]
  
  # ------------------------------------------------------------
  # Interpolate missing periods
  # ------------------------------------------------------------
  
  dataAgriPrice <- as.quitte(dataAgriPrice) %>%
    mutate(
      period = as.integer(period)
    ) %>%
    interpolate_missing_periods(
      period = fStartHorizon:fEndY,
      expand.values = TRUE
    ) %>%
    as.magpie()
  
  
  # ------------------------------------------------------------
  # Standardise BMSWAS name
  # ------------------------------------------------------------
  
  getNames(BMSWAS) <- "AG.kUSD2015/toe.BMSWAS"
  
  
  # ------------------------------------------------------------
  # Convert AGFuelPrice from USD2015/toe to kUSD2015/toe
  # ------------------------------------------------------------
  
  AGFuelPrice <- AGFuelPrice / 1000
  
  getNames(AGFuelPrice) <- gsub(
    "USD2015/toe",
    "kUSD2015/toe",
    getNames(AGFuelPrice),
    fixed = TRUE
  )
  
  
  # ------------------------------------------------------------
  # Start with AGFuelPrice as fallback
  # ------------------------------------------------------------
  
  newAgriPrice <- AGFuelPrice
  
  
  # ------------------------------------------------------------
  # Replace BMSWAS
  # BMSWAS has priority, AGFuelPrice is fallback
  # ------------------------------------------------------------
  
  v <- getNames(BMSWAS)
  
  newAgriPrice[, , v] <- ifelse(
    is.na(BMSWAS[, , v]),
    newAgriPrice[, , v],
    BMSWAS[, , v]
  )
  
  
  # ------------------------------------------------------------
  # Replace with Eurostat agricultural prices
  # Eurostat has priority
  # AGFuelPrice is fallback where available
  # ------------------------------------------------------------
  
  r <- getRegions(dataAgriPrice)
  
  for (v in getNames(dataAgriPrice)) {
    
    # If Eurostat fuel does not exist in AGFuelPrice,
    # add it as NA for all countries/years
    if (!v %in% getNames(newAgriPrice)) {
      
      tmp <- new.magpie(
        cells_and_regions = getRegions(newAgriPrice),
        years             = getYears(newAgriPrice),
        names             = v,
        fill              = NA
      )
      
      newAgriPrice <- mbind(
        newAgriPrice,
        tmp
      )
    }
    
    # Eurostat has priority.
    # Where Eurostat is NA, retain AGFuelPrice/fallback value.
    euro <- dataAgriPrice[r, , v]
    fallback <- newAgriPrice[r, , v]
    
    newAgriPrice[r, , v] <- ifelse(
      is.na(euro),
      fallback,
      euro
    )
  }
  
  getItems(newAgriPrice, 3.2) <- "USD2015/toe"
  newAgriPrice <- newAgriPrice * 1000
  
  x <- as.quitte(newAgriPrice)
  
  # complete incomplete time series
  x <- as.quitte(x) %>%
    interpolate_missing_periods(period = fStartHorizon : fEndY, expand.values = TRUE) 
  
  x <- x %>%
    group_by(variable, period, unit, fuel) %>%
    mutate(
      value = ifelse(
        is.na(value),
        mean(value, na.rm = TRUE),
        value
      )
    ) %>%
    ungroup() %>% as.quitte() %>% as.magpie()
  
  # -------------- weights -------------------------------------------
  # ------------------------------------------------------------------
  # Calculation of aggregation weights
  
  Population <- calcOutput("POP", aggregate = FALSE)
  Population <- Population[, getYears(x), , drop = TRUE]
  
  weights <- x
  weights[, , ] <- Population
  
  list(
    x = x,
    weight = weights,
    unit = "various",
    description = "AGPrices",
    mixed_aggregation = TRUE
  )
}
