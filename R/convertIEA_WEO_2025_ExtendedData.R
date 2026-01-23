#' convertIEA_WEO_2025_ExtendedData
#'
#' @param x MAgPIE object.
#'
#' @return The "IEA_WEO_2025_ExtendedData" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_WEO_2025_ExtendedData", subtype = "IEA_WEO_2025_ExtendedData", convert = TRUE)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter %>% mutate select full_join arrange group_by distinct intersect setdiff ungroup group_map 
#' @importFrom tidyr pivot_wider

convertIEA_WEO_2025_ExtendedData <- function(x) {
  
  horizon <-c(2010:2100)
  
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  IEA_WEO <- x
  Historical <- IEA_WEO[,c(2010,2015,2023,2024),"Historical"][,,"Total"]
  Current <- IEA_WEO[,c(2035,2040,2045,2050),"Current Policies Scenario"][,,"Total"]
  Current <- collapseDim(Current ,3.2)
  Historical <- collapseDim(Historical ,3.2)
  IEA_WEO <- mbind(Historical, Current)
  
  # take year after 2022 because these data is only for projections
  IEA_WEO <- IEA_WEO[,as.numeric(getYears(IEA_WEO, as.integer = TRUE)) > 2022,]
  
  IEA_WEO <- as.quitte(IEA_WEO) %>%
    interpolate_missing_periods(period = 2023:2050, expand.values = TRUE)
  
  # this map matches regions of WEO with 249 open-prom regions
  WEO_region_map <- toolGetMapping(
    name = "WEO_region_map.csv",
    type = "regional",
    where = "mrprom"
  )
  
  IEA_WEO <- as.quitte(IEA_WEO) %>% as.magpie()
  
  IEA_WEO <- IEA_WEO[unique(WEO_region_map[["Region"]]),,]
  
  IEA_WEO_249 <- toolAggregate(IEA_WEO, dim = 1, rel = WEO_region_map, from = "Region", to = "ISO3.Code")
  
  #find trend, period-to-period relative change (growth rate)
  IEA_WEO_249 <- as.quitte(IEA_WEO_249) %>%
    arrange(region, variable, period, category) %>%
    group_by(region, variable, category) %>%
    mutate(
      base_2023 = value[period == 2023][1],                # store 2023 value
      value = if_else(
        period >= 2024,
        (value - base_2023) / base_2023,                  # growth rate relative to 2023
        value                                             # historical values unchanged
      )
    ) %>%
    ungroup() %>%
    select(-base_2023)
  
  IEA_WEO_249 <- select(IEA_WEO_249, c("region", "variable", "unit", "period", "category", "value"))
  
  IEA_WEO_249 <- as.quitte(IEA_WEO_249) %>% as.magpie()
  
  # use trends for projections so take projections(after 2023)
  IEA_WEO_249 <- IEA_WEO_249[,as.numeric(getYears(IEA_WEO_249, as.integer = TRUE)) > 2023,]
  
  # from WEO take each subsector
  Industry <- IEA_WEO_249[,,"Industry"][,,"PJ"]
  getItems(Industry, 3) <- "Final Energy|Industry"
  names(dimnames(Industry))[3] <- "variable"
  Industry <- as.quitte(Industry)
  
  Transportation <- IEA_WEO_249[,,"Transport"][,,"PJ"]
  getItems(Transportation, 3) <- "Final Energy|Transportation"
  names(dimnames(Transportation))[3] <- "variable"
  Transportation <- as.quitte(Transportation)
  
  FE <- IEA_WEO_249[,,"Total final consumption"][,,"PJ"]
  getItems(FE, 3) <- "Final Energy"
  names(dimnames(FE))[3] <- "variable"
  FE <- as.quitte(FE)
  
  # load historical data to multiply the trends to calculate actual values
  INDSE <- calcOutput(type = "IFuelCons2", subtype = "INDSE", aggregate = FALSE)
  INDSE <- dimSums(INDSE, 3)
  getItems(INDSE, 3) <- "Final Energy|Industry"
  names(dimnames(INDSE))[3] <- "variable"
  INDSE <- as.quitte(INDSE)
  
  TRANSE_FuelCons <- calcOutput(type = "IFuelCons2", subtype = "TRANSE", aggregate = FALSE)
  TRANSE <- dimSums(TRANSE_FuelCons, 3)
  getItems(TRANSE, 3) <- "Final Energy|Transportation"
  names(dimnames(TRANSE))[3] <- "variable"
  TRANSE <- as.quitte(TRANSE)
  
  ALL_FE <- calcOutput(type = "IFuelCons2", subtype = "ALL", aggregate = FALSE)
  ALL_FE <- dimSums(ALL_FE, 3)
  getItems(ALL_FE, 3) <- "Final Energy"
  names(dimnames(ALL_FE))[3] <- "variable"
  ALL_FE <- as.quitte(ALL_FE)
  
  df <- rbind(INDSE, Industry, TRANSE, Transportation, FE, ALL_FE)
  
  # multiply trends with historical data (starts from 2023 until 2050)
  dataIEA <- df %>%
    arrange(region, variable, period) %>%
    group_by(region, variable) %>%
    mutate(
      base_2023 = value[period == 2023][1],            # get 2023 value per group
      value = if_else(
        period >= 2024,
        base_2023 * (1 + value),                      # apply growth relative to 2023
        value                                         # keep historical as-is
      )
    ) %>%
    ungroup() %>%
    select(-base_2023)
  
  dataIEA <- as.quitte(dataIEA) %>% as.magpie()
  dataIEA <- dataIEA[,as.numeric(getYears(dataIEA, as.integer = TRUE)) > 2023,]
  
  dataIEA[is.na(dataIEA)] <- 0
  
  total <- mbind(NULL, dataIEA)

  # from WEO take sec energy elec
  SE <- IEA_WEO_249[,,"Electricity generation"][,,"TWh"]
  getItems(SE, 3) <- "Secondary Energy|Electricity"
  names(dimnames(SE))[3] <- "variable"
  SE <- as.quitte(SE)
  
  # load historical data from IEA
  ELOUTPUT <- readSource("IEA2025", subset = c("ELOUTPUT"))
  ELOUTPUTworld <- readSource("IEA2025", subset = c("ELOUTPUT"), convert = FALSE)[,getYears(ELOUTPUT),]
  ELOUTPUTworld <- ELOUTPUTworld["WORLD",,]
  ELOUTPUT <- mbind(ELOUTPUT, ELOUTPUTworld)
  ELOUTPUT <- ELOUTPUT[,,"GWH"]
  years_in_horizon <-  horizon[horizon %in% getYears(ELOUTPUT, as.integer = TRUE)]
  ELOUTPUT <- ELOUTPUT[,years_in_horizon,]
  ELOUTPUT <- ELOUTPUT[,,"TOTAL"]
  ELOUTPUT <- collapseDim(ELOUTPUT ,3.1)
  ELOUTPUT <- collapseDim(ELOUTPUT ,3.1)
  ELOUTPUT <- ELOUTPUT / 1000 #to TWh
  ELOUTPUT[is.na(ELOUTPUT)] <- 0
  ELOUTPUTworld <- ELOUTPUT["WORLD",,]
  
  ELOUTPUT <- ELOUTPUT[getRegions(ELOUTPUT)[getRegions(ELOUTPUT) %in% as.character(getISOlist())], , ]
  
  ELOUTPUT_249 <- ELOUTPUT
  
  getItems(ELOUTPUT_249, 3) <- "Secondary Energy|Electricity"
  names(dimnames(ELOUTPUT_249))[3] <- "variable"
  ELOUTPUT_249 <- as.quitte(ELOUTPUT_249)
  
  df <- rbind(SE, ELOUTPUT_249)
  
  # multiply trends with historical data (starts from 2023 until 2050)
  dataIEA <- df %>%
    arrange(region, variable, period) %>%
    group_by(region, variable) %>%
    mutate(
      base_2023 = value[period == 2023][1],            # get 2023 value per group
      value = if_else(
        period >= 2024,
        base_2023 * (1 + value),                      # apply growth relative to 2023
        value                                         # keep historical as-is
      )
    ) %>%
    ungroup() %>%
    select(-base_2023)
  
  dataIEA <- as.quitte(dataIEA) %>% as.magpie()
  dataIEA <- dataIEA[,as.numeric(getYears(dataIEA, as.integer = TRUE)) > 2023,]
  
  dataIEA[is.na(dataIEA)] <- 0
  
  total <- mbind(total, dataIEA)
  
  #############  co2 from 2022
  IEA_WEO <- x
  Historical <- IEA_WEO[,c(2010,2015,2023,2024),"Historical"][,,"Total"]
  Current <- IEA_WEO[,c(2035,2040,2045,2050),"Current Policies Scenario"][,,"Total"]
  Current <- collapseDim(Current ,3.2)
  Historical <- collapseDim(Historical ,3.2)
  IEA_WEO <- mbind(Historical, Current)
  
  # take data after 2021 because historical data of EDGAR stops at 2022
  IEA_WEO <- IEA_WEO[,as.numeric(getYears(IEA_WEO, as.integer = TRUE)) > 2021,]
  
  IEA_WEO <- as.quitte(IEA_WEO) %>%
    interpolate_missing_periods(period = 2022:2050, expand.values = TRUE)
  
  IEA_WEO <- as.quitte(IEA_WEO) %>% as.magpie()
  
  IEA_WEO <- IEA_WEO[unique(WEO_region_map[["Region"]]),,]
  
  # from WEO regions to 249
  IEA_WEO_249_2023 <- toolAggregate(IEA_WEO, dim = 1, rel = WEO_region_map, from = "Region", to = "ISO3.Code")
  
  # find trend, period-to-period relative change (growth rate)
  IEA_WEO_249_2023 <- as.quitte(IEA_WEO_249_2023) %>%
    arrange(region, variable, period, category) %>%
    group_by(region, variable, category) %>%
    mutate(
      base_2022 = value[period == 2022][1],                # store 2022 value
      value = if_else(
        period >= 2023,
        (value - base_2022) / base_2022,                  # growth rate relative to 2022
        value                                             # historical values unchanged
      )
    ) %>%
    ungroup() %>%
    select(-base_2022)
  
  IEA_WEO_249_2023 <- select(IEA_WEO_249_2023, c("region", "variable", "unit", "period", "category", "value"))
  
  IEA_WEO_249_2023 <- as.quitte(IEA_WEO_249_2023) %>% as.magpie()
  
  # load emissions co2
  CO2 <- IEA_WEO_249_2023[,,"Total final consumption"][,,"Mt CO2"]
  CO2 <- CO2[,as.numeric(getYears(IEA_WEO, as.integer = TRUE)) > 2022,]
  getItems(CO2, 3) <- "Emissions|CO2"
  names(dimnames(CO2))[3] <- "variable"
  CO2 <- as.quitte(CO2)
  
  # take EDGAR for historical data to multiply with trends
  EDGAR <- readSource("EDGAR", convert = TRUE)
  EDGAR <- dimSums(EDGAR, 3)
  getItems(EDGAR, 3) <- "Emissions|CO2"
  names(dimnames(EDGAR))[3] <- "variable"
  EDGAR <- as.quitte(EDGAR)
  
  df <- rbind(CO2, EDGAR)
  
  # multiply trends with historical data
  dataIEA <- df %>%
    arrange(region, variable, period) %>%
    group_by(region, variable) %>%
    mutate(
      base_2022 = value[period == 2022][1],            # get 2023 value per group
      value = if_else(
        period >= 2023,
        base_2022 * (1 + value),                      # apply growth relative to 2023
        value                                         # keep historical as-is
      )
    ) %>%
    ungroup() %>%
    select(-base_2022)
  
  dataIEA <- as.quitte(dataIEA) %>% as.magpie()
  dataIEA <- dataIEA[,as.numeric(getYears(dataIEA, as.integer = TRUE)) > 2022,]
  
  dataIEA[is.na(dataIEA)] <- 0
  
  total <- mbind(total, dataIEA[,getYears(total),])
  
  ############# trasport per fuel
  # load fuels transport
  IEA_WEO <- x
  Historical <- IEA_WEO[,c(2010,2015,2023,2024),"Historical"][,,c("Electricity","Total liquids","Total gases","Oil","Natural gas")][,,"Transport"]
  Current <- IEA_WEO[,c(2035,2040,2045,2050),"Current Policies Scenario"][,,c("Electricity","Total liquids","Total gases","Oil","Natural gas")][,,"Transport"]
  Current <- collapseDim(Current ,3.2)
  Historical <- collapseDim(Historical ,3.2)
  IEA_WEO <- mbind(Historical, Current)
  
  # data for projections after 2022
  IEA_WEO <- IEA_WEO[,as.numeric(getYears(IEA_WEO, as.integer = TRUE)) > 2022,]
  
  IEA_WEO <- as.quitte(IEA_WEO) %>%
    interpolate_missing_periods(period = 2023:2050, expand.values = TRUE)
  
  IEA_WEO <- as.quitte(IEA_WEO) %>% as.magpie()
  
  IEA_WEO <- IEA_WEO[unique(WEO_region_map[["Region"]]),,]
  
  # from WEO regions to open prom regions
  IEA_WEO_249 <- toolAggregate(IEA_WEO, dim = 1, rel = WEO_region_map, from = "Region", to = "ISO3.Code")
  
  IEA_WEO_249 <- collapseDim(IEA_WEO_249,c(3.1,3.2,3.3,3.5))
  
  # calculate open prom the fuels from fuels of WEO
  BGDO <- IEA_WEO_249[,,"Total liquids"] - IEA_WEO_249[,,"Oil"]
  getItems(BGDO,3) <- "BGDO"
  names(dimnames(BGDO))[3] <- "variable"
  BGSL <- IEA_WEO_249[,,"Total liquids"] - IEA_WEO_249[,,"Oil"]
  getItems(BGSL,3) <- "BGSL"
  names(dimnames(BGSL))[3] <- "variable"
  GDO <- IEA_WEO_249[,,"Oil"]
  getItems(GDO,3) <- "GDO"
  names(dimnames(GDO))[3] <- "variable"
  GSL <- IEA_WEO_249[,,"Oil"]
  getItems(GSL,3) <- "GSL"
  names(dimnames(GSL))[3] <- "variable"
  H2F <- IEA_WEO_249[,,"Total gases"] - IEA_WEO_249[,,"Natural gas"]
  getItems(H2F,3) <- "H2F"
  names(dimnames(H2F))[3] <- "variable"
  OGS <- IEA_WEO_249[,,"Total gases"] - IEA_WEO_249[,,"Natural gas"]
  getItems(OGS,3) <- "OGS"
  names(dimnames(OGS))[3] <- "variable"
  NGS <- IEA_WEO_249[,,"Natural gas"]
  getItems(NGS,3) <- "NGS"
  names(dimnames(NGS))[3] <- "variable"
  ELC <- IEA_WEO_249[,,"Electricity"]
  getItems(ELC,3) <- "ELC"
  names(dimnames(ELC))[3] <- "variable"
  
  Transport <- mbind(ELC,BGDO,GDO,H2F,OGS,NGS,GSL,BGSL)
  
  #find trend, period-to-period relative change (growth rate)
  Transport <- as.quitte(Transport) %>%
    arrange(region, variable, period) %>%
    group_by(region, variable) %>%
    mutate(
      base_2023 = value[period == 2023][1],                # store 2023 value
      value = if_else(
        period >= 2024,
        (value - base_2023) / base_2023,                  # growth rate relative to 2023
        value                                             # historical values unchanged
      )
    ) %>%
    ungroup() %>%
    select(-base_2023)
  
  Transport <- select(Transport, c("region", "variable", "unit", "period", "value"))
  
  Transport <- as.quitte(Transport) %>% as.magpie()
  
  Transport <- Transport[,as.numeric(getYears(Transport, as.integer = TRUE)) > 2023,]
  
  TRANSE <- TRANSE_FuelCons
  TRANSE <- dimSums(TRANSE, 3.1)
  
  # add H2F to historical data with value 0
  TRANSE <- add_columns(TRANSE, addnm = "H2F", dim = 3, fill = 0)
  
  TRANSE <- TRANSE[,,getItems(Transport, 3)]
  
  Transportation <- mbind(Transport, TRANSE)
  
  getItems(Transportation, 3) <- paste0("Final Energy|Transportation|",getItems(Transportation, 3))

  df <- as.quitte(Transportation)
  
  # multiply trends with historical data
  # H2F is a new technology that “appears” after 2023
  # Example (H2F):
  #   
  # 2023 level = 0
  # 
  # 2024 “growth” = 5
  # 
  # Output = 5
  
  dataIEA <- df %>%
    arrange(region, variable, period) %>%
    group_by(region, variable) %>%
    mutate(
      base_2023 = value[period == 2023][1],
      value = case_when(
        period >= 2024 & base_2023 == 0 ~ value,                 # additive
        period >= 2024 ~ base_2023 * (1 + value),                # multiplicative
        TRUE ~ value
      )
    ) %>%
    ungroup() %>%
    select(-base_2023)
  
  dataIEA <- as.quitte(dataIEA) %>% as.magpie()
  dataIEA <- dataIEA[,as.numeric(getYears(dataIEA, as.integer = TRUE)) > 2023,]
  
  dataIEA[is.na(dataIEA)] <- 0
  
  total <- mbind(total, dataIEA)
  
  return(total)
  
}
