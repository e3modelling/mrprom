#' fullVALIDATION2
#'
#' Read data from PRIMES and EMBER convert it to a mrprom mif
#' file so to compare output mif file with OPEN-PROM output.
#'
#' @return The mif file
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- retrieveData("fullVALIDATION2", regionmapping = "regionmappingOPDEV3.csv")
#' }
#'
#' @importFrom quitte as.quitte write.mif
#' @importFrom dplyr select filter %>% left_join mutate across group_by
#' @importFrom tidyr separate_rows separate_longer_delim separate_wider_delim drop_na
#' @importFrom stringr str_remove str_remove_all
#' @importFrom utils  read.csv
#' @export

fullVALIDATION2 <- function() {
  
  # Read regional mapping
  rmap <- toolGetMapping(getConfig("regionmapping"), "regional", where = "mrprom")
  
  horizon <-c(2010:2100)
  
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  ######### Primes_BALANCES
  Primes_BALANCES <- calcOutput(type = "Primes", aggregate = FALSE)
  
  years_in_horizon <-  horizon[horizon %in% getYears(Primes_BALANCES, as.integer = TRUE)]
  
  Primes_BALANCES <- collapseDim(Primes_BALANCES, 3.2)
  
  # OPEN-PROM sectors
  sector <- c("TRANSE", "INDSE", "DOMSE", "NENSE")
  sector_name <- c("Transportation", "Industry", "Residential and Commercial", "Non Energy and Bunkers")
  
  Primes_BALANCES_agg <- NULL
  for (y in 1 : length(sector)) {
    
    sets6 <- toolGetMapping(paste0(sector[y], ".csv"),
                            type = "blabla_export",
                            where = "mrprom")
    Primes_BALANCES_SECTORAL <- Primes_BALANCES[,,getItems(Primes_BALANCES,3.1)[getItems(Primes_BALANCES,3.1) %in% sets6[,1]]]
    getItems(Primes_BALANCES_SECTORAL, 3.1) <- paste0("Final Energy|", sector_name[y],"|", getItems(Primes_BALANCES_SECTORAL, 3.1))
    Primes_BALANCES_agg <- mbind(Primes_BALANCES_agg, Primes_BALANCES_SECTORAL)
  }
  
  # remove . from magpie object and replace with |
  Primes_BALANCES_agg <- as.quitte(Primes_BALANCES_agg)
  Primes_BALANCES_agg[[names(Primes_BALANCES_agg[, 4])]] <- paste0(Primes_BALANCES_agg[[names(Primes_BALANCES_agg[, 4])]], "|", Primes_BALANCES_agg[["new"]])
  Primes_BALANCES_agg <- select(Primes_BALANCES_agg, -c("new"))
  Primes_BALANCES_agg <- as.quitte(Primes_BALANCES_agg) %>% as.magpie()
  
  # aggregation
  Primes_BALANCES_agg[is.na(Primes_BALANCES_agg)] <- 0
  Primes_BALANCES_agg <- toolAggregate(Primes_BALANCES_agg, rel = rmap)
  
  # write data in mif file
  write.report(Primes_BALANCES_agg[, years_in_horizon, ],file = "reporting.mif", model = "Primes_BALANCES", unit = "Mtoe", append = TRUE, scenario = "Validation")
  #####################
  
  #Primes Secondary Energy Electricity data
  PrimesSE <- readSource("PrimesPGData", subtype = "SE")
  
  PrimesSE <- PrimesSE[getRegions(PrimesSE)[getRegions(PrimesSE) %in% as.character(getISOlist())], , ]
  
  PrimesSE <-  as.quitte(PrimesSE) %>%
    interpolate_missing_periods(period = getYears(PrimesSE,as.integer=TRUE)[1]:getYears(PrimesSE,as.integer=TRUE)[length(getYears(PrimesSE))], expand.values = TRUE)
  
  PrimesSE <- as.quitte(PrimesSE) %>% as.magpie()
  
  PrimesSE <- PrimesSE[,fStartHorizon:max(getYears(PrimesSE, as.integer = TRUE)),]
  
  PrimesSE <- PrimesSE[intersect(getRegions(PrimesSE),rmap[,3]),,]
  PrimesSE <- collapseDim(PrimesSE, 3.1)
  
  years_in_horizon <-  horizon[horizon %in% getYears(PrimesSE, as.integer = TRUE)]
  
  # aggregation
  PrimesSE[is.na(PrimesSE)] <- 0
  PrimesSE <- toolCountryFill(PrimesSE, fill = 0)
  PrimesSE <- toolAggregate(PrimesSE, rel = rmap)
  
  # write data in mif file
  write.report(PrimesSE[, years_in_horizon, ],file = "reporting.mif", model = "PrimesSE", unit = "TWh", append = TRUE, scenario = "Validation")
  ###############
  
  ######### Ember Capacity
  
  rename_EF <- c(
    "HCL" = "Hard coal",
    "GDO" = "Oil",
    "NGS" = "Gas",
    "BMSWAS" = "Biomass",
    "NUC" = "Nuclear",
    "HYD" = "Hydro",
    "WND" = "Wind",
    "SOL" = "Solar",
    "GEO" = "Geothermal"
  )
  
  df_rename_EF <- data.frame(
    Code = names(rename_EF),
    Fuel = as.vector(rename_EF),
    stringsAsFactors = FALSE
  )
  
  EmberCapacity <- getEmberCap() %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value"))
  
  EmberCapacity <- as.quitte(EmberCapacity) %>% as.magpie()
  
  PGALLtoEF <- toolGetMapping(name = "PGALLtoEF.csv",
                              type = "blabla_export",
                              where = "mrprom")
  
  PGALLtoEF <- PGALLtoEF[PGALLtoEF[,"PGALL"] %in% intersect(getItems(EmberCapacity,3),PGALLtoEF[,1]),]
  
  EmberCapacity <- toolAggregate(EmberCapacity, dim = 3, rel = PGALLtoEF, from = "PGALL", to = "EF")
  EmberCapacity <- toolAggregate(EmberCapacity, dim = 3, rel = df_rename_EF, from = "Code", to = "Fuel")
  
  getItems(EmberCapacity, 3) <- paste0("Capacity|Electricity","|", getItems(EmberCapacity, 3))
  
  years_in_horizon <-  horizon[horizon %in% getYears(EmberCapacity, as.integer = TRUE)]
  
  # aggregation
  EmberCapacity[is.na(EmberCapacity)] <- 0
  EmberCapacity <- toolAggregate(EmberCapacity, rel = rmap)
  
  EmberCapacity_GLO <- dimSums(EmberCapacity, 1)
  getItems(EmberCapacity_GLO, 1) <- "World"
  EmberCapacity <- mbind(EmberCapacity, EmberCapacity_GLO)
  
  EmberCapacity_total <- dimSums(EmberCapacity, 3)
  getItems(EmberCapacity_total, 3) <- "Capacity|Electricity"
  EmberCapacity <- mbind(EmberCapacity, EmberCapacity_total)
  
  # write data in mif file
  write.report(EmberCapacity[, years_in_horizon, ],file = "reporting.mif", model = "EmberCapacity", unit = "GW", append = TRUE, scenario = "historical")
  
  ################
  
  ######### EmberSE
  
  rename_EF <- c(
    "HCL" = "Hard coal",
    "GDO" = "Oil",
    "NGS" = "Gas",
    "BMSWAS" = "Biomass",
    "NUC" = "Nuclear",
    "HYD" = "Hydro",
    "WND" = "Wind",
    "SOL" = "Solar",
    "GEO" = "Geothermal"
  )
  
  df_rename_EF <- data.frame(
    Code = names(rename_EF),
    Fuel = as.vector(rename_EF),
    stringsAsFactors = FALSE
  )
  
  EmberSE <- getEmberProdElec() %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value"))
  
  EmberSE <- as.quitte(EmberSE) %>% as.magpie()
  
  PGALLtoEF <- toolGetMapping(name = "PGALLtoEF.csv",
                              type = "blabla_export",
                              where = "mrprom")
  
  PGALLtoEF <- PGALLtoEF[PGALLtoEF[,"PGALL"] %in% intersect(getItems(EmberSE,3),PGALLtoEF[,1]),]
  
  EmberSE <- toolAggregate(EmberSE, dim = 3, rel = PGALLtoEF, from = "PGALL", to = "EF")
  EmberSE <- toolAggregate(EmberSE, dim = 3, rel = df_rename_EF, from = "Code", to = "Fuel")
  
  getItems(EmberSE, 3) <- paste0("Secondary Energy|Electricity","|", getItems(EmberSE, 3))
  
  years_in_horizon <-  horizon[horizon %in% getYears(EmberSE, as.integer = TRUE)]
  
  # aggregation
  EmberSE[is.na(EmberSE)] <- 0
  EmberSE <- toolAggregate(EmberSE, rel = rmap)
  
  EmberSE_GLO <- dimSums(EmberSE, 1)
  getItems(EmberSE_GLO, 1) <- "World"
  EmberSE <- mbind(EmberSE, EmberSE_GLO)
  
  EmberSE_total <- dimSums(EmberSE, 3)
  getItems(EmberSE_total, 3) <- "Secondary Energy|Electricity"
  EmberSE <- mbind(EmberSE, EmberSE_total)
  
  # write data in mif file
  write.report(EmberSE[, years_in_horizon, ],file = "reporting.mif", model = "EmberSE", unit = "TWh", append = TRUE, scenario = "historical")
  
  ################
  
  # Pik emissions
  pik <- readSource("PIK", convert = TRUE)
  pik <- pik[,,"Energy.MtCO2.CO2"]
  getItems(pik, 3) <- paste0("Emissions|CO2")
  
  # aggregation
  pik[is.na(pik)] <- 0
  pik <- toolAggregate(pik, rel = rmap)
  
  pik <- as.quitte(pik) %>%
    interpolate_missing_periods(period = getYears(pik,as.integer=TRUE)[1]:getYears(pik,as.integer=TRUE)[length(getYears(pik))], expand.values = TRUE)
  
  pik <- as.quitte(pik) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(pik, as.integer = TRUE)]
  
  pik_GLO <- dimSums(pik, 1)
  getItems(pik_GLO, 1) <- "World"
  pik <- mbind(pik, pik_GLO)
  
  # write data in mif file
  write.report(pik[, years_in_horizon, ], file = "reporting.mif", model = "PIK", unit = "Mt CO2/yr", append = TRUE, scenario = "historical")
  
  ##############
  # IEA_CO2, EDGAR emissions
  EDGAR <- calcOutput(type = "CO2_emissions", aggregate = TRUE)
  getItems(EDGAR, 3) <- paste0("Emissions|CO2")
  
  EDGAR <- as.quitte(EDGAR) %>%
    interpolate_missing_periods(period = getYears(EDGAR,as.integer=TRUE)[1]:getYears(EDGAR,as.integer=TRUE)[length(getYears(EDGAR))], expand.values = TRUE)
  
  EDGAR <- as.quitte(EDGAR) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(EDGAR, as.integer = TRUE)]
  
  EDGAR_GLO <- dimSums(EDGAR, 1)
  getItems(EDGAR_GLO, 1) <- "World"
  EDGAR <- mbind(EDGAR, EDGAR_GLO)
  
  write.report(EDGAR[, years_in_horizon, ], file = "reporting.mif", model = "IEA_CO2, EDGAR", unit = "Mt CO2/yr", append=TRUE, scenario = "historical")
  #########################
  dataIEA <- readSource("IEA2025", subset = c("TFC","TOTIND","TOTTRANS"))
  dataIEAworld <- readSource("IEA2025", subset = c("TFC","TOTIND","TOTTRANS"), convert = FALSE)
  dataIEAworld <- dataIEAworld["IEAFAMILY",,]
  dataIEA <- mbind(dataIEA, dataIEAworld)
  dataIEA <- dataIEA[,,"KTOE"]
  years_in_horizon <-  horizon[horizon %in% getYears(dataIEA, as.integer = TRUE)]
  dataIEA <- dataIEA[,years_in_horizon,]
  dataIEA <- dataIEA[,,"TOTAL"]
  dataIEA <- collapseDim(dataIEA ,3.1)
  dataIEA <- collapseDim(dataIEA ,3.1)
  dataIEA <- dataIEA / 1000 #to Mtoe
  dataIEA[is.na(dataIEA)] <- 0
  dataIEA_world <- dataIEA["IEAFAMILY",,]
  
  dataIEA <- dataIEA[getRegions(dataIEA)[getRegions(dataIEA) %in% as.character(getISOlist())], , ]
    
  # aggregation
  dataIEA <- toolAggregate(dataIEA, rel = rmap)
  #add world
  getItems(dataIEA_world, 1) <- "World"
  dataIEA <- mbind(dataIEA, dataIEA_world)
  
  getItems(dataIEA, 3) <- c("Final Energy", "Final Energy|Industry", "Final Energy|Transportation")
  
  # write data in mif file
  write.report(dataIEA,file = "reporting.mif", model = "IEA_CONS_TOTAL", unit = "Mtoe", append = TRUE, scenario = "historical")
  #############################################
  
  ######################### FINAL ENERGY PER SUBSECTOR
  sbsIEAtoPROM <- toolGetMapping(
    name = "prom-iea-sbs.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(c("IEA", "OPEN.PROM"), sep = ",") %>%
    rename(flow = IEA)
  
  sbsIEAtoPROM <- sbsIEAtoPROM[!(sbsIEAtoPROM[["flow"]] %in% c("ROAD", "RAIL", "DOMESNAV")),]
  
  dataFuelCons <- readSource("IEA2025", subset = unique(sbsIEAtoPROM$flow))
  dataFuelConsworld <- readSource("IEA2025", subset = unique(sbsIEAtoPROM$flow), convert = FALSE)
  
  dataFuelConsworld <- dataFuelConsworld["IEAFAMILY",,]
  dataFuelCons <- mbind(dataFuelCons, dataFuelConsworld)
  dataFuelCons <- dataFuelCons[,,"KTOE"]
  years_in_horizon <-  horizon[horizon %in% getYears(dataFuelCons, as.integer = TRUE)]
  dataFuelCons <- dataFuelCons[,years_in_horizon,]
  dataFuelCons <- dataFuelCons[,,"TOTAL"]
  dataFuelCons <- collapseDim(dataFuelCons ,3.1)
  dataFuelCons <- collapseDim(dataFuelCons ,3.1)
  dataFuelCons <- dataFuelCons / 1000 #to Mtoe
  dataFuelCons[is.na(dataFuelCons)] <- 0
  dataFuelConsworld <- dataFuelCons["IEAFAMILY",,]
  
  dataFuelCons <- dataFuelCons[getRegions(dataFuelCons)[getRegions(dataFuelCons) %in% as.character(getISOlist())], , ]
  
  # aggregation
  dataFuelCons <- toolAggregate(dataFuelCons, rel = rmap)
  #add world
  getItems(dataFuelConsworld, 1) <- "World"
  dataFuelCons <- mbind(dataFuelCons, dataFuelConsworld)
  
  # change names
  dataFuelCons <- toolAggregate(dataFuelCons, rel = sbsIEAtoPROM, dim = 3, from = "flow", to = "OPEN.PROM")
  
  dataFuelCons_INDSE <- dataFuelCons[,,c("IS","NF","CH","BM","PP","FD","EN","TX","OE","OI")]
  dataFuelCons_DOMSE <- dataFuelCons[,,c("SE", "AG", "HOU")]
  dataFuelCons_NENSE <- dataFuelCons[,,c("NEN","BU","PCH")]
  dataFuelCons_TRANSE <- dataFuelCons[,,c("PA")]
  
  getItems(dataFuelCons_INDSE, 3) <- paste0("Final Energy|Industry|", getItems(dataFuelCons_INDSE, 3))
  getItems(dataFuelCons_TRANSE, 3) <- paste0("Final Energy|Transportation|", getItems(dataFuelCons_TRANSE, 3))
  getItems(dataFuelCons_DOMSE, 3) <- paste0("Final Energy|Residential and Commercial|", getItems(dataFuelCons_DOMSE, 3))
  getItems(dataFuelCons_NENSE, 3) <- paste0("Final Energy|Non Energy and Bunkers|", getItems(dataFuelCons_NENSE, 3))
  
  dataFuelCons <- mbind(dataFuelCons_INDSE, dataFuelCons_TRANSE, dataFuelCons_DOMSE, dataFuelCons_NENSE)
  
  # write data in mif file
  write.report(dataFuelCons,file = "reporting.mif", model = "IEA_CONS_TOTAL", unit = "Mtoe", append = TRUE, scenario = "historical")
  #############################################
  
  # rename mif file
  fullVALIDATION2 <- read.report("reporting.mif")
  write.report(fullVALIDATION2, file = paste0("fullVALIDATION2.mif"))
  
  return(list(x = NULL,
              weight = NULL,
              unit = NULL,
              description = "VALIDATION2"))
  
}

# Helpers---------------------------------------------------
getSharesTech <- function(take_shares, techProd_data, vars) {
  shares <- take_shares %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value")) %>%
    filter(variable %in% vars) %>%
    group_by(region, period) %>% # Group by region and period
    mutate(total_value = sum(value), share = value / total_value) %>%
    select(c("region", "variable", "period", "share")) %>%
    replace_na(list(share = 0)) %>%
    right_join(techProd_data, by = c("region", "period", "variable")) %>%
    mutate(value = value * share) %>%
    select(-share) %>%
    ungroup()
  return(shares)
}

getEmberCap <- function() {
  capacities <- readSource("EMBER", convert = TRUE)
  capacities <- capacities[, , "Capacity"]
  capacities <- collapseDim(capacities, 3.3)
  
  mapEMBER <- data.frame(
    EMBER = c(
      "Bioenergy", "Coal", "Gas", "Hydro", "Nuclear", "Other Fossil",
      "Other Renewables", "Solar", "Wind"
    ),
    OPEN_PROM = c(
      "ATHBMSWAS", "ATHCOAL", "ATHGAS", "PGLHYD", "PGANUC", "ATHOIL",
      "PGOTHREN", "PGSOL", "PGAWND"
    )
  )
  
  # aggregate from ENERDATA fuels to reporting fuel categories
  capacities <- toolAggregate(capacities, dim = 3.1, rel = mapEMBER, from = "EMBER", to = "OPEN_PROM")
  
  techProd <- as.quitte(capacities) %>% as.magpie()
  
  # Set NA to 0
  techProd[is.na(techProd)] <- 0
  return(techProd)
}


getEmberProdElec <- function() {
  power <- readSource("EMBER", convert = TRUE)
  power <- power[, , "Electricity generation"]
  power <- collapseDim(power, 3.3)
  
  mapEMBER <- data.frame(
    EMBER = c(
      "Bioenergy", "Coal", "Gas", "Hydro", "Nuclear", "Other Fossil",
      "Other Renewables", "Solar", "Wind"
    ),
    OPEN_PROM = c(
      "ATHBMSWAS", "ATHCOAL", "ATHGAS", "PGLHYD", "PGANUC", "ATHOIL",
      "PGOTHREN", "PGSOL", "PGAWND"
    )
  )
  
  # aggregate from ENERDATA fuels to reporting fuel categories
  power <- toolAggregate(power, dim = 3.1, rel = mapEMBER, from = "EMBER", to = "OPEN_PROM")
  
  techProd <- as.quitte(power) %>% as.magpie()
  
  # Set NA to 0
  techProd[is.na(techProd)] <- 0
  return(techProd)
}

