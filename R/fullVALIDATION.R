#' fullVALIDATION
#'
#' Read data from MENA-EDS, ENERDATA, IEA and NAVIGATE convert it to a mrprom mif
#' file so to compare output mif file with OPEN-PROM output.
#'
#' @return The mif file
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- retrieveData("VALIDATION", regionmapping = "regionmappingOPDEV3.csv")
#' }
#'
#' @importFrom quitte as.quitte write.mif
#' @importFrom dplyr select filter %>% left_join mutate across group_by
#' @importFrom tidyr separate_rows separate_longer_delim separate_wider_delim
#' @importFrom stringr str_remove str_remove_all
#' @importFrom utils  read.csv
#' @export

fullVALIDATION <- function() {
  
  # Read regional mapping
  rmap <- toolGetMapping(getConfig("regionmapping"), "regional", where = "mrprom")
  
  # Read MENA-PROM mapping, will use it to choose the correct variables from MENA
  map <- toolGetMapping(name = "MENA-PROM mapping - mena_prom_mapping.csv",
                        type = "sectoral",
                        where = "mrprom")
  
  horizon <-c(2010:2100)
  
  ######### reportFinalEnergy
  # read GAMS set used for reporting of Final Energy
  sets <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "BALEF2EFS")
  sets[, 1] <- gsub("\"", "", sets[, 1])
  sets <- separate_wider_delim(sets,cols = 1, delim = ".", names = c("BAL", "EF"))
  sets[["EF"]] <- sub("\\(", "", sets[["EF"]])
  sets[["EF"]] <- sub("\\)", "", sets[["EF"]])
  EF <- NULL
  sets <- separate_rows(sets, EF)
  sets[["BAL"]] <- gsub("Gas fuels", "Gases", sets[["BAL"]])
  
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]

  # add model MENA_EDS data (choosing the correct variable from MENA by use of the MENA-PROM mapping)
  # Total final energy consumnption (Mtoe)
  MENA_EDS_VFeCons <- readSource("MENA_EDS", subtype =  map[map[["OPEN.PROM"]] == "VConsFinEneCountry", "MENA.EDS"])
  getRegions(MENA_EDS_VFeCons) <- sub("MOR", "MAR", getRegions(MENA_EDS_VFeCons)) # fix wrong region names in MENA
  
  lastYear <- sub("y", "", tail(sort(getYears(MENA_EDS_VFeCons)), 1))
  MENA_EDS_VFeCons <- MENA_EDS_VFeCons[, c(fStartHorizon : lastYear), ]
  
  # choose years and regions that both models have
  years <- getYears(MENA_EDS_VFeCons)
  
  # aggregate from MENA fuels to reporting fuel categories
  MENA_EDS_VFeCons <- toolAggregate(MENA_EDS_VFeCons[, years, unique(sets[["EF"]])],dim = 3,rel = sets,from = "EF",to = "BAL")
  
  # complete names
  getItems(MENA_EDS_VFeCons, 3) <- paste0("Final Energy|", getItems(MENA_EDS_VFeCons, 3))
  
  l <- getNames(MENA_EDS_VFeCons) == "Final Energy|Total"
  getNames(MENA_EDS_VFeCons)[l] <- "Final Energy"
  
  MENA_EDS_VFeCons <- as.quitte(MENA_EDS_VFeCons) %>%
    interpolate_missing_periods(period = getYears(MENA_EDS_VFeCons,as.integer=TRUE)[1]:getYears(MENA_EDS_VFeCons,as.integer=TRUE)[length(getYears(MENA_EDS_VFeCons))], expand.values = TRUE)
  
  MENA_EDS_VFeCons <- as.quitte(MENA_EDS_VFeCons) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(MENA_EDS_VFeCons, as.integer = TRUE)]
  
  MENA_EDS_VFeCons_GLO <- dimSums(MENA_EDS_VFeCons, 1)
  getItems(MENA_EDS_VFeCons_GLO, 1) <- "World"
  MENA_EDS_VFeCons <- mbind(MENA_EDS_VFeCons, MENA_EDS_VFeCons_GLO)
  
  # write data in mif file
  write.report(MENA_EDS_VFeCons[, years_in_horizon, ], file = "reporting.mif", model = "MENA-EDS", unit = "Mtoe",append = TRUE, scenario = "Baseline")
  
  # filter ENERDATA by consumption
  consumption_ENERDATA <- readSource("ENERDATA", subtype =  "consumption", convert = TRUE)
  # filter ENERDATA by Electricity own use of energy industries
  own_use_enerdata <- readSource("ENERDATA", subtype =  "own", convert = TRUE)
  own_use_enerdata <- own_use_enerdata[,, "Electricity own use of energy industries.Mtoe"]
  
  # map of enerdata and balance fuel
  map_enerdata <- toolGetMapping(name = "enerdata-by-fuel.csv",
                                 type = "sectoral",
                                 where = "mrprom")
  
  # choose years that both models have
  year <- Reduce(intersect, list(getYears(MENA_EDS_VFeCons, as.integer = TRUE), getYears(consumption_ENERDATA, as.integer=TRUE)))
  
  # keep the variables from the map
  consumption_ENERDATA_variables <- consumption_ENERDATA[, year, map_enerdata[, 1]]
  
  # from Mt to Mtoe
  l <- getNames(consumption_ENERDATA_variables) == "Jet fuel final consumption.Mt"
  getNames(consumption_ENERDATA_variables)[l] <- "Jet fuel final consumption.Mtoe"
  consumption_ENERDATA_variables[, , "Jet fuel final consumption.Mtoe"] <- consumption_ENERDATA_variables[, , "Jet fuel final consumption.Mtoe"] / 1.027
  
  # remove Electricity own use of energy industries
  consumption_ENERDATA_variables[, , "Electricity final consumption.Mtoe"] <- consumption_ENERDATA_variables[, , "Electricity final consumption.Mtoe"] - ifelse(is.na(own_use_enerdata[, year, ]), 0, own_use_enerdata[, year, ])
  consumption_ENERDATA_variables <- as.quitte(consumption_ENERDATA_variables)
  names(map_enerdata) <- sub("ENERDATA", "variable", names(map_enerdata))
  
  # remove units
  map_enerdata[,1] <- sub("\\..*", "", map_enerdata[,1])
  
  # add a column with the fuels that match each variable of enerdata
  v <- left_join(consumption_ENERDATA_variables, map_enerdata, by = "variable")
  v["variable"] <- paste0("Final Energy|", v[["fuel"]])
  v <- filter(v, period %in% year)
  v <- select(v , -c("fuel"))
  v <- as.quitte(v)
  v <- as.magpie(v)
  v[is.na(v)] <- 0
  v <- toolAggregate(v, rel = rmap)
  
  l <- getNames(v) == "Final Energy|Total.Mtoe"
  getNames(v)[l] <- "Final Energy.Mtoe"
  
  v <- as.quitte(v) %>%
    interpolate_missing_periods(period = getYears(v, as.integer = TRUE), expand.values = TRUE)
  
  v <- as.quitte(v) %>% as.magpie()
  
  v <- as.quitte(v) %>%
    interpolate_missing_periods(period = getYears(v,as.integer=TRUE)[1]:getYears(v,as.integer=TRUE)[length(getYears(v))], expand.values = TRUE)
  
  v <- as.quitte(v) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(v, as.integer = TRUE)]
  
  v_GLO <- dimSums(v, 1)
  getItems(v_GLO, 1) <- "World"
  v <- mbind(v, v_GLO)
  
  # write data in mif file
  write.report(v[, years_in_horizon, ],file = "reporting.mif", model = "ENERDATA", unit = "Mtoe", append = TRUE, scenario = "Validation")
  
  # map of IEA and balance fuel
  map_IEA <- toolGetMapping(name = "IEA_projections.csv",
                            type = "sectoral",
                            where = "mrprom")
  
  # read dataset IEA_Energy_Projections_Balances
  IEA_all_dataset <- readSource("IEA_Energy_Projections_Balances", subtype = "all")
  year_IEA <- Reduce(intersect, list(getYears(MENA_EDS_VFeCons, as.integer=TRUE), getYears(IEA_all_dataset, as.integer=TRUE)))
  IEA_Balances <- IEA_all_dataset[, year_IEA, "STEPS"][, , map_IEA[!is.na(map_IEA[, 2]), 2]][, , map_IEA[!is.na(map_IEA[, 2]), 3]]
  IEA_Balances <- as.quitte(IEA_Balances)
  
  # add a column with the fuels that match each variable of IEA
  names(map_IEA) <- sub("PRODUCT", "product", names(map_IEA))
  IEA_FC <- left_join(IEA_Balances, map_IEA, by = "product")
  IEA_FC["variable"] <- IEA_FC["OPEN.PROM"]
  IEA_FC <- filter(IEA_FC, period %in% year_IEA)
  IEA_FC <- select(IEA_FC , -c("OPEN.PROM", "FLOW", "product", "flow"))
  IEA_FC <- as.quitte(IEA_FC)
  IEA_FC <- unique(IEA_FC)
  IEA_FC <- as.magpie(IEA_FC)
  IEA_FC[is.na(IEA_FC)] <- 0
  IEA_FC <- toolAggregate(IEA_FC, rel = rmap)
  
  IEA_FC <- as.quitte(IEA_FC) %>%
    interpolate_missing_periods(period = getYears(IEA_FC,as.integer=TRUE)[1]:getYears(IEA_FC,as.integer=TRUE)[length(getYears(IEA_FC))], expand.values = TRUE)
  
  IEA_FC <- as.quitte(IEA_FC) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(IEA_FC, as.integer = TRUE)]
  
  IEA_FC_GLO <- dimSums(IEA_FC, 1)
  getItems(IEA_FC_GLO, 1) <- "World"
  IEA_FC <- mbind(IEA_FC, IEA_FC_GLO)
  
  # write data in mif file
  write.report(IEA_FC[, years_in_horizon, ],file = "reporting.mif", model = "IEA_projections", unit = "Mtoe", append = TRUE)
  
  # Final Energy | "TRANSE" | "INDSE" | "DOMSE" | "NENSE"
  
  # Link between Model Subsectors and Fuels
  sets4 <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "SECTTECH")
  sets4[6,] <- paste0(sets4[6,] , sets4[7,])
  sets4 <- sets4[ - c(7),,drop = FALSE]
  sets4[7,] <- paste0(sets4[7,] , sets4[8,], sets4[9,])
  sets4 <- sets4[ - c(8, 9),,drop = FALSE]
  sets4 <- separate_wider_delim(sets4,cols = 1, delim = ".", names = c("SBS","EF"))
  sets4[["EF"]] <- sub("\\(","",sets4[["EF"]])
  sets4[["EF"]] <- sub("\\)","",sets4[["EF"]])
  sets4[["SBS"]] <- sub("\\(","",sets4[["SBS"]])
  sets4[["SBS"]] <- sub("\\)","",sets4[["SBS"]])
  sets4 <- separate_rows(sets4,EF)
  SBS <- NULL
  sets4 <- separate_rows(sets4,SBS)
  sets4 <- filter(sets4, EF != "")
  
  # OPEN-PROM sectors
  sector <- c("TRANSE", "INDSE", "DOMSE", "NENSE")
  sector_name <- c("Transportation", "Industry", "Residential and Commercial", "Non Energy and Bunkers")
  
  # variables of OPEN-PROM related to sectors
  blabla_var <- c("VDemFinEneTranspPerFuel", "VConsFuel", "VConsFuel", "VConsFuel")
  
  for (y in 1 : length(sector)) {
    # read GAMS set used for reporting of Final Energy different for each sector
    sets6 <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), sector[y])
    
    sets6 <- separate_rows(sets6, paste0(sector[y], "(DSBS)"))
    sets6 <- as.data.frame(sets6)
    
    map_subsectors <- sets4 %>% filter(SBS %in% as.character(sets6[, 1]))
    map_subsectors_by_sector <- map_subsectors
    
    map_subsectors[["EF"]] = paste(map_subsectors[["SBS"]], map_subsectors[["EF"]], sep=".")
    
    # add model MENA_EDS data (choosing the correct variable from MENA by use of the MENA-PROM mapping)
    FCONS_by_sector_and_EF_MENA_EDS <- readSource("MENA_EDS", subtype =  map[map[["OPEN.PROM"]] == blabla_var[y], "MENA.EDS"])
    getRegions(FCONS_by_sector_and_EF_MENA_EDS) <- sub("MOR", "MAR", getRegions(FCONS_by_sector_and_EF_MENA_EDS)) # fix wrong region names in MENA
    
    # choose years and regions that both models have
    years <- getYears(MENA_EDS_VFeCons, as.integer=TRUE)
    
    # aggregate from PROM fuels to subsectors
    FCONS_by_sector_MENA <- toolAggregate(FCONS_by_sector_and_EF_MENA_EDS[, , unique(map_subsectors[["EF"]])], dim = 3,rel = map_subsectors, from = "EF", to = "SBS")
    getItems(FCONS_by_sector_MENA, 3) <- paste0("Final Energy|", sector_name[y],"|", getItems(FCONS_by_sector_MENA, 3))
    
    FCONS_by_sector_MENA <- as.quitte(FCONS_by_sector_MENA) %>%
      interpolate_missing_periods(period = getYears(FCONS_by_sector_MENA,as.integer=TRUE)[1]:getYears(FCONS_by_sector_MENA,as.integer=TRUE)[length(getYears(FCONS_by_sector_MENA))], expand.values = TRUE)
    
    FCONS_by_sector_MENA <- as.quitte(FCONS_by_sector_MENA) %>% as.magpie()
    years_in_horizon <-  horizon[horizon %in% getYears(FCONS_by_sector_MENA, as.integer = TRUE)]
    
    FCONS_by_sector_MENA_GLO <- dimSums(FCONS_by_sector_MENA, 1)
    getItems(FCONS_by_sector_MENA_GLO, 1) <- "World"
    FCONS_by_sector_MENA <- mbind(FCONS_by_sector_MENA, FCONS_by_sector_MENA_GLO)
    
    # write data in mif file
    write.report(FCONS_by_sector_MENA[, years_in_horizon, ], file = "reporting.mif", model = "MENA-EDS",unit = "Mtoe", append = TRUE, scenario = "Baseline")
    
    # Final Energy by sector
    sector_mena <- dimSums(FCONS_by_sector_MENA, dim = 3, na.rm = TRUE)
    getItems(sector_mena, 3) <- paste0("Final Energy|", sector_name[y])
    
    sector_mena <- as.quitte(sector_mena) %>%
      interpolate_missing_periods(period = getYears(sector_mena,as.integer=TRUE)[1]:getYears(sector_mena,as.integer=TRUE)[length(getYears(sector_mena))], expand.values = TRUE)
    
    sector_mena <- as.quitte(sector_mena) %>% as.magpie()
    years_in_horizon <-  horizon[horizon %in% getYears(sector_mena, as.integer = TRUE)]
    
    # write data in mif file
    write.report(sector_mena[, years_in_horizon, ],file = "reporting.mif", model = "MENA-EDS", unit = "Mtoe", append = TRUE, scenario = "Baseline")
    
    # Energy Forms Aggregations
    sets5 <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "EFtoEFA")
    sets5[5,] <- paste0(sets5[5, ] , sets5[6, ])
    sets5 <- sets5[- 6, ]
    sets5 <- as.data.frame(sets5)
    sets5 <- separate_wider_delim(sets5, cols = 1, delim = ".", names = c("EF", "EFA"))
    sets5[["EF"]] <- sub("\\(", "", sets5[["EF"]])
    sets5[["EF"]] <- sub("\\)", "", sets5[["EF"]])
    sets5 <- separate_rows(sets5, EF)
    
    # Add electricity, Hydrogen, Biomass and Waste
    ELC <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "ELCEF")
    sets5[nrow(sets5) + 1, ] <- ELC[1, 1]
    
    sets10 <- sets5
    
    # per fuel
    FCONS_per_fuel_mena <- FCONS_by_sector_and_EF_MENA_EDS[, , sets6[, 1]]
    
    # remove . from magpie object and replace with |
    FCONS_per_fuel_mena <- as.quitte(FCONS_per_fuel_mena)
    FCONS_per_fuel_mena[[names(FCONS_per_fuel_mena[, 8])]] <- paste0(FCONS_per_fuel_mena[[names(FCONS_per_fuel_mena[, 8])]], "|", FCONS_per_fuel_mena[["EF"]])
    FCONS_per_fuel_mena <- select(FCONS_per_fuel_mena, -c("EF"))
    FCONS_per_fuel_mena <- as.quitte(FCONS_per_fuel_mena) %>% as.magpie()
    getItems(FCONS_per_fuel_mena, 3) <- paste0("Final Energy|", sector_name[y],"|", getItems(FCONS_per_fuel_mena, 3))
    
    FCONS_per_fuel_mena <- as.quitte(FCONS_per_fuel_mena) %>%
      interpolate_missing_periods(period = getYears(FCONS_per_fuel_mena,as.integer=TRUE)[1]:getYears(FCONS_per_fuel_mena,as.integer=TRUE)[length(getYears(FCONS_per_fuel_mena))], expand.values = TRUE)
    
    FCONS_per_fuel_mena <- as.quitte(FCONS_per_fuel_mena) %>% as.magpie()
    years_in_horizon <-  horizon[horizon %in% getYears(FCONS_per_fuel_mena, as.integer = TRUE)]
    
    FCONS_per_fuel_mena_GLO <- dimSums(FCONS_per_fuel_mena, 1)
    getItems(FCONS_per_fuel_mena_GLO, 1) <- "World"
    FCONS_per_fuel_mena <- mbind(FCONS_per_fuel_mena, FCONS_per_fuel_mena_GLO)
    
    # write data in mif file
    write.report(FCONS_per_fuel_mena[,years_in_horizon,],file="reporting.mif",model = "MENA-EDS",unit = "Mtoe", append = TRUE, scenario = "Baseline")
    
    # Aggregate model MENA_EDS by subsector and by energy form
    by_energy_form_and_by_subsector_mena <- toolAggregate(FCONS_by_sector_and_EF_MENA_EDS[, , sets6[, 1]][, , as.character(unique(sets10[["EF"]]))], dim = 3.2, rel = sets10, from = "EF", to = "EFA")
    
    # sector by subsector and by energy form MENA_EDS
    mena_by_subsector_by_energy_form <- by_energy_form_and_by_subsector_mena
    getItems(mena_by_subsector_by_energy_form, 3.1) <- paste0("Final Energy|", sector_name[y],"|", getItems(mena_by_subsector_by_energy_form, 3.1))
    
    # remove . from magpie object
    mena_by_subsector_by_energy_form <- as.quitte(mena_by_subsector_by_energy_form)
    mena_by_subsector_by_energy_form[[names(mena_by_subsector_by_energy_form[, 8])]] <- paste0(mena_by_subsector_by_energy_form[[names(mena_by_subsector_by_energy_form[, 8])]], "|", mena_by_subsector_by_energy_form[[names(mena_by_subsector_by_energy_form[, 9])]])
    mena_by_subsector_by_energy_form <- select(mena_by_subsector_by_energy_form, -c(names(mena_by_subsector_by_energy_form[, 9])))
    mena_by_subsector_by_energy_form <- as.quitte(mena_by_subsector_by_energy_form) %>% as.magpie()
    
    mena_by_subsector_by_energy_form <- as.quitte(mena_by_subsector_by_energy_form) %>%
      interpolate_missing_periods(period = getYears(mena_by_subsector_by_energy_form,as.integer=TRUE)[1]:getYears(mena_by_subsector_by_energy_form,as.integer=TRUE)[length(getYears(mena_by_subsector_by_energy_form))], expand.values = TRUE)
    
    mena_by_subsector_by_energy_form <- as.quitte(mena_by_subsector_by_energy_form) %>% as.magpie()
    years_in_horizon <-  horizon[horizon %in% getYears(mena_by_subsector_by_energy_form, as.integer = TRUE)]
    
    mena_by_subsector_by_energy_form_GLO <- dimSums(mena_by_subsector_by_energy_form, 1)
    getItems(mena_by_subsector_by_energy_form_GLO, 1) <- "World"
    mena_by_subsector_by_energy_form <- mbind(mena_by_subsector_by_energy_form, mena_by_subsector_by_energy_form_GLO)
    
    # write data in mif file
    write.report(mena_by_subsector_by_energy_form[, years_in_horizon, ], file = "reporting.mif", model = "MENA-EDS", unit = "Mtoe", append = TRUE, scenario = "Baseline")
    
    # sector_by_energy_form
    by_energy_form_mena <- dimSums(by_energy_form_and_by_subsector_mena, 3.1, na.rm = TRUE)
    getItems(by_energy_form_mena, 3.1) <- paste0("Final Energy|", sector_name[y], "|", getItems(by_energy_form_mena, 3.1))
    
    by_energy_form_mena <- as.quitte(by_energy_form_mena) %>%
      interpolate_missing_periods(period = getYears(by_energy_form_mena,as.integer=TRUE)[1]:getYears(by_energy_form_mena,as.integer=TRUE)[length(getYears(by_energy_form_mena))], expand.values = TRUE)
    
    by_energy_form_mena <- as.quitte(by_energy_form_mena) %>% as.magpie()
    years_in_horizon <-  horizon[horizon %in% getYears(by_energy_form_mena, as.integer = TRUE)]
    
    by_energy_form_mena_GLO <- dimSums(by_energy_form_mena, 1)
    getItems(by_energy_form_mena_GLO, 1) <- "World"
    by_energy_form_mena <- mbind(by_energy_form_mena, by_energy_form_mena_GLO)
    
    # write data in mif file
    write.report(by_energy_form_mena[, years_in_horizon, ],file = "reporting.mif", model = "MENA-EDS", unit = "Mtoe", append = TRUE, scenario = "Baseline")
    
    ###  USE READSOURCE INSTEAD CALCOUTPUT
    # load data source (ENERDATA)
    subtp <- sector[y]
    x <- readSource("ENERDATA", "consumption", convert = TRUE)
    
    # filter years
    fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
    lastYear <- sub("y", "", tail(sort(getYears(x)), 1))
    x <- x[, c(fStartHorizon : lastYear), ]
    
    # load current OPENPROM set configuration
    sets <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), subtp)
    sets <- unlist(strsplit(sets[, 1], ","))
    
    # use enerdata-openprom mapping to extract correct data from source
    map_fc <- toolGetMapping(name = "prom-enerdata-fucon-mapping.csv",
                             type = "sectoral",
                             where = "mrprom")
    maps <- map_fc
    
    ## filter mapping to keep only XXX sectors
    map_fc <- filter(map_fc, map_fc[, "SBS"] %in% sets)
    
    ## ..and only items that have an enerdata-prom mapping
    enernames <- unique(map_fc[!is.na(map_fc[, "ENERDATA"]), "ENERDATA"])
    map_fc <- map_fc[map_fc[, "ENERDATA"] %in% enernames, ]
    
    ## filter data to keep only XXX data
    enernames <- unique(map_fc[!is.na(map_fc[, "ENERDATA"]), "ENERDATA"])
    x <- x[, , enernames]
    
    ## for oil, rename unit from Mt to Mtoe
    if (any(grepl("oil", getItems(x, 3.1)) & grepl("Mt$", getNames(x)))) {
      tmp <- x[, , "Mt"]
      getItems(tmp, 3.2) <- "Mtoe"
      x <- mbind(x[, , "Mtoe"], tmp)
      map_fc[["ENERDATA"]] <-  sub(".Mt$", ".Mtoe", map_fc[["ENERDATA"]])
    }
    
    ## rename variables to openprom names
    out <- NULL
    
    ## rename variables from ENERDATA to openprom names
    ff <- paste(map_fc[, 2], map_fc[, 3], sep = ".")
    iii <- 0
    
    ### add a dummy dimension to data because mapping has 3 dimensions, and data only 2
    for (ii in map_fc[, "ENERDATA"]) {
      iii <- iii + 1
      out <- mbind(out, setNames(add_dimension(x[, , ii], dim = 3.2), paste0(ff[iii], ".", sub("^.*.\\.", "", getNames(x[, , ii])))))
    }
    x <- out
    
    if (subtp == "TRANSE") {
      
      a6 <- readSource("IRF", subtype = "inland-surface-passenger-transport-by-rail")
      #million pKm/yr
      a7 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-rail")
      #million tKm/yr
      a6 <- a6[, Reduce(intersect, list(getYears(a6), getYears(a7), getYears(x))), ]
      a7 <- a7[, Reduce(intersect, list(getYears(a6), getYears(a7), getYears(x))), ]
      x <- x[, Reduce(intersect, list(getYears(a6), getYears(a7), getYears(x))), ]
      
      out3 <- (a6 / (a6 + a7))
      out4 <- (a7 / (a6 + a7))
      
      #inland-surface-passenger-transport-by-rail / total inland-surface transport-by-rail
      x[, , "PT.GDO.Mtoe"] <- x[, , "PT.GDO.Mtoe"] * ifelse(is.na(out3), mean(out3, na.rm=TRUE), out3)
      #inland-surface-freight-transport-by-rail / total inland-surface
      x[, , "GT.GDO.Mtoe"] <- x[, , "GT.GDO.Mtoe"] * ifelse(is.na(out4), mean(out4, na.rm=TRUE), out4)
      
      x[, , "PT.ELC.Mtoe"] <- x[, , "PT.ELC.Mtoe"] * ifelse(is.na(out3), mean(out3, na.rm=TRUE), out3)
      x[, , "GT.ELC.Mtoe"] <- x[, , "GT.ELC.Mtoe"] * ifelse(is.na(out4), mean(out4, na.rm=TRUE), out4)
      
      a8 <- readSource("IRF", subtype = "passenger-car-traffic")
      #million motor vehicles Km/yr
      a9 <- readSource("IRF", subtype = "total-van,-pickup,-lorry-and-road-tractor-traffic")
      #million motor vehicles Km/yr
      a8 <- a8[, Reduce(intersect, list(getYears(a8), getYears(a9), getYears(x))), ]
      a9 <- a9[, Reduce(intersect, list(getYears(a8), getYears(a9), getYears(x))), ]
      x <- x[, Reduce(intersect, list(getYears(a8), getYears(a9), getYears(x))), ]
      
      out1 <- (a8 / (a8 + a9))
      out2 <- (a9 / (a8 + a9))
      
      #inland-surface-freight-transport-by-road / total inland-surface-transport-by-road
      x[, , "PC.GDO.Mtoe"] <- x[, , "PC.GDO.Mtoe"] * ifelse(is.na(out1), mean(out1, na.rm=TRUE), out1)
      x[, , "PC.GSL.Mtoe"] <- x[, , "PC.GSL.Mtoe"] * ifelse(is.na(out1), mean(out1, na.rm=TRUE), out1)
      x[, , "GU.GDO.Mtoe"] <- x[, , "GU.GDO.Mtoe"] * ifelse(is.na(out2), mean(out2, na.rm=TRUE), out2)
      
      l <- getNames(x) == "PA.KRS.Mt"
      getNames(x)[l] <- "PA.KRS.Mtoe"
      #from Mt to Mtoe
      x[,,"PA.KRS.Mtoe"] <- x[,,"PA.KRS.Mtoe"] / 1.027
    }
    
    if (sector[y] == "INDSE") {
      #OI is FE total per fuel - the sum of the other subsectors per fuel
      sum_subsectors <- dimSums(x[,,getItems(x,3.1)[!(getItems(x,3.1) %in% "OI")]][,,getItems(x[,,"OI"],3.3)], dim = 3.1, na.rm = TRUE)
      sum_subsectors <- as.quitte(sum_subsectors)
      sum_subsectors["variable"] <- "OI"
      sum_subsectors <- sum_subsectors[, c(1, 2, 3, 4, 8 , 5 , 6 , 7)]
      sum_subsectors <- as.quitte(sum_subsectors)
      sum_subsectors <- as.magpie(sum_subsectors)
      sum_subsectors[,2021,] <- sum_subsectors[,2020,]
      x[,,"OI"] <- x[,,"OI"] - sum_subsectors
      x[x < 0] <- 10^-6
      x[,,"OI"][,,"NGS"][x[,,"OI"][,,"NGS"] == 0] <- 10^-6
    }
    
    x[is.na(x)] <- 10^-6
    
    FuelCons_enerdata <- x
    
    year <- Reduce(intersect, list(getYears(FCONS_by_sector_MENA, as.integer=TRUE), getYears(FuelCons_enerdata, as.integer = TRUE)))
    FuelCons_enerdata <- FuelCons_enerdata[, year, ]
    
    map_subsectors_ener <- sets4 %>% filter(SBS %in% as.character(sets6[, 1]))
    
    map_subsectors_ener[["EF"]] = paste(map_subsectors_ener[["SBS"]], "Mtoe",map_subsectors_ener[["EF"]], sep=".")
    
    # filter to have only the variables which are in enerdata
    FuelCons_enerdata <- as.quitte(FuelCons_enerdata)
    FuelCons_enerdata <- as.magpie(FuelCons_enerdata)
    map_subsectors_ener <- map_subsectors_ener %>% filter(EF %in% getItems(FuelCons_enerdata, 3))
    
    # aggregate from enerdata fuels to subsectors
    enerdata_by_sector <- toolAggregate(FuelCons_enerdata[, , as.character(unique(map_subsectors_ener[["EF"]]))], dim = 3, rel = map_subsectors_ener, from = "EF", to = "SBS")
    getItems(enerdata_by_sector, 3) <- paste0("Final Energy|", sector_name[y], "|", getItems(enerdata_by_sector, 3))
    
    # country aggregation
    enerdata_by_sector[is.na(enerdata_by_sector)] <- 0
    enerdata_by_sector <- toolAggregate(enerdata_by_sector, rel = rmap)
    
    enerdata_by_sector <- as.quitte(enerdata_by_sector) %>%
      interpolate_missing_periods(period = getYears(enerdata_by_sector,as.integer=TRUE)[1]:getYears(enerdata_by_sector,as.integer=TRUE)[length(getYears(enerdata_by_sector))], expand.values = TRUE)
    
    enerdata_by_sector <- as.quitte(enerdata_by_sector) %>% as.magpie()
    years_in_horizon <-  horizon[horizon %in% getYears(enerdata_by_sector, as.integer = TRUE)]
    
    enerdata_by_sector_GLO <- dimSums(enerdata_by_sector, 1)
    getItems(enerdata_by_sector_GLO, 1) <- "World"
    enerdata_by_sector <- mbind(enerdata_by_sector, enerdata_by_sector_GLO)
    
    # write data in mif file
    write.report(enerdata_by_sector[, years_in_horizon, ], file = "reporting.mif", model = "ENERDATA", unit = "Mtoe", append = TRUE, scenario = "Validation")
    
    # Final Energy enerdata
    FE_ener <- dimSums(enerdata_by_sector, dim = 3, na.rm = TRUE)
    getItems(FE_ener, 3) <- paste0("Final Energy|", sector_name[y])
    
    FE_ener <- as.quitte(FE_ener) %>%
      interpolate_missing_periods(period = getYears(FE_ener,as.integer=TRUE)[1]:getYears(FE_ener,as.integer=TRUE)[length(getYears(FE_ener))], expand.values = TRUE)
    
    FE_ener <- as.quitte(FE_ener) %>% as.magpie()
    years_in_horizon <-  horizon[horizon %in% getYears(FE_ener, as.integer = TRUE)]
    
    # write data in mif file
    write.report(FE_ener[, years_in_horizon, ], file = "reporting.mif", model = "ENERDATA", unit = "Mtoe", append = TRUE, scenario = "Validation")
    
    # per fuel
    FCONS_per_fuel_enerdata <- FuelCons_enerdata[, , sets6[, 1]]
    
    # remove . from magpie object and replace with |
    FCONS_per_fuel_enerdata <- as.quitte(FCONS_per_fuel_enerdata)
    FCONS_per_fuel_enerdata[[names(FCONS_per_fuel_enerdata[, 4])]] <- paste0(FCONS_per_fuel_enerdata[[names(FCONS_per_fuel_enerdata[, 4])]], "|", FCONS_per_fuel_enerdata[["new"]])
    FCONS_per_fuel_enerdata <- select(FCONS_per_fuel_enerdata, -c("new","unit"))
    FCONS_per_fuel_enerdata <- as.quitte(FCONS_per_fuel_enerdata) %>% as.magpie()
    getItems(FCONS_per_fuel_enerdata, 3) <- paste0("Final Energy|", sector_name[y],"|", getItems(FCONS_per_fuel_enerdata, 3))
    
    FCONS_per_fuel_enerdata <- as.quitte(FCONS_per_fuel_enerdata) %>%
      interpolate_missing_periods(period = getYears(FCONS_per_fuel_enerdata,as.integer=TRUE)[1]:getYears(FCONS_per_fuel_enerdata,as.integer=TRUE)[length(getYears(FCONS_per_fuel_enerdata))], expand.values = TRUE)
    
    FCONS_per_fuel_enerdata <- as.quitte(FCONS_per_fuel_enerdata) %>% as.magpie()
    years_in_horizon <-  horizon[horizon %in% getYears(FCONS_per_fuel_enerdata, as.integer = TRUE)]
    
    FCONS_per_fuel_enerdata <- toolAggregate(FCONS_per_fuel_enerdata, rel = rmap)
    
    FCONS_per_fuel_enerdata_GLO <- dimSums(FCONS_per_fuel_enerdata, 1)
    getItems(FCONS_per_fuel_enerdata_GLO, 1) <- "World"
    FCONS_per_fuel_enerdata <- mbind(FCONS_per_fuel_enerdata, FCONS_per_fuel_enerdata_GLO)
    
    # write data in mif file
    write.report(FCONS_per_fuel_enerdata[,years_in_horizon,],file="reporting.mif",model = "ENERDATA", unit = "Mtoe", append = TRUE, scenario = "Validation")
    
    map_subsectors_ener2 <- sets10
    # filter to have only the variables which are in enerdata
    map_subsectors_ener2 <- map_subsectors_ener2 %>% filter(EF %in% getItems(FuelCons_enerdata, 3.3))
    
    # Aggregate model enerdata by subsector and by energy form
    enerdata_by_EF_and_sector <- toolAggregate(FuelCons_enerdata[, year, as.character(unique(map_subsectors_ener2[["EF"]]))], dim = 3.3, rel = map_subsectors_ener2, from = "EF",to = "EFA")
    
    # enerdata by subsector and by energy form
    enerdata_by_subsector_by_energy_form <- enerdata_by_EF_and_sector
    enerdata_by_subsector_by_energy_form <- dimSums(enerdata_by_subsector_by_energy_form, 3.2, na.rm = TRUE)
    getItems(enerdata_by_subsector_by_energy_form, 3.1) <- paste0("Final Energy|", sector_name[y], "|", getItems(enerdata_by_subsector_by_energy_form, 3.1))
    
    # country aggregation
    enerdata_by_subsector_by_energy_form[is.na(enerdata_by_subsector_by_energy_form)] <- 0
    enerdata_by_subsector_by_energy_form <- toolAggregate(enerdata_by_subsector_by_energy_form, rel = rmap)
    
    # remove . from magpie object
    enerdata_by_subsector_by_energy_form <- as.quitte(enerdata_by_subsector_by_energy_form)
    enerdata_by_subsector_by_energy_form[["variable"]] <- paste0(enerdata_by_subsector_by_energy_form[["variable"]], "|", enerdata_by_subsector_by_energy_form[["new"]])
    enerdata_by_subsector_by_energy_form <- select(enerdata_by_subsector_by_energy_form, -c("new"))
    enerdata_by_subsector_by_energy_form <- as.quitte(enerdata_by_subsector_by_energy_form) %>% as.magpie()
    
    enerdata_by_subsector_by_energy_form <- as.quitte(enerdata_by_subsector_by_energy_form) %>%
      interpolate_missing_periods(period = getYears(enerdata_by_subsector_by_energy_form,as.integer=TRUE)[1]:getYears(enerdata_by_subsector_by_energy_form,as.integer=TRUE)[length(getYears(enerdata_by_subsector_by_energy_form))], expand.values = TRUE)
    
    enerdata_by_subsector_by_energy_form <- as.quitte(enerdata_by_subsector_by_energy_form) %>% as.magpie()
    years_in_horizon <-  horizon[horizon %in% getYears(enerdata_by_subsector_by_energy_form, as.integer = TRUE)]
    
    enerdata_by_subsector_by_energy_form_GLO <- dimSums(enerdata_by_subsector_by_energy_form, 1)
    getItems(enerdata_by_subsector_by_energy_form_GLO, 1) <- "World"
    enerdata_by_subsector_by_energy_form <- mbind(enerdata_by_subsector_by_energy_form, enerdata_by_subsector_by_energy_form_GLO)
    
    # write data in mif file
    write.report(enerdata_by_subsector_by_energy_form[, years_in_horizon, ], file = "reporting.mif", model = "ENERDATA", unit = "Mtoe", append = TRUE, scenario = "Validation")
    
    # Aggregate model enerdata by energy form
    enerdata_by_energy_form <- dimSums(enerdata_by_EF_and_sector, 3.1, na.rm = TRUE)
    getItems(enerdata_by_energy_form,3) <- paste0("Final Energy|", sector_name[y], "|", getItems(enerdata_by_energy_form, 3.2))
    
    # country aggregation
    enerdata_by_energy_form[is.na(enerdata_by_energy_form)] <- 0
    enerdata_by_energy_form <- toolAggregate(enerdata_by_energy_form, rel = rmap)
    
    enerdata_by_energy_form <- as.quitte(enerdata_by_energy_form) %>%
      interpolate_missing_periods(period = getYears(enerdata_by_energy_form,as.integer=TRUE)[1]:getYears(enerdata_by_energy_form,as.integer=TRUE)[length(getYears(enerdata_by_energy_form))], expand.values = TRUE)
    
    enerdata_by_energy_form <- as.quitte(enerdata_by_energy_form) %>% as.magpie()
    years_in_horizon <-  horizon[horizon %in% getYears(enerdata_by_energy_form, as.integer = TRUE)]
    
    enerdata_by_energy_form_GLO <- dimSums(enerdata_by_energy_form, 1)
    getItems(enerdata_by_energy_form_GLO, 1) <- "World"
    enerdata_by_energy_form <- mbind(enerdata_by_energy_form, enerdata_by_energy_form_GLO)
    
    # write data in mif file
    write.report(enerdata_by_energy_form[, years_in_horizon, ], file = "reporting.mif", model = "ENERDATA", unit = "Mtoe", append = TRUE, scenario = "Validation")
    
    # Add IEA data from world balances
    IEA <- NULL
    map_IEA <- maps %>% drop_na(IEA)
    IEA_WB <- NULL
    map_IEA <- map_IEA %>% filter(SBS %in% map_subsectors_by_sector[["SBS"]])
    
    # the map has a column SBS which corresponds to flow of IEA
    for (ii in unique(map_IEA[, "flow"])) {
      d <- readSource("IEA", subtype = as.character(ii))
      d <- d / 1000 #ktoe to mtoe
      d <- as.quitte(d)
      # each flow has some products as it is the EF column of map
      m <- filter(map_IEA, map_IEA[["flow"]] == ii)
      # for each product of IEA data
      region <- NULL
      period <- NULL
      product <- NULL
      flow <- NULL
      
      qb <- filter(d, product %in% m[, 4])
      qb <- select((qb), c(region, period, value, product, flow))
      
      if (ii == "MARBUNK") {
        qb["value"] <- - qb["value"]
      }
      
      qb <- filter(qb, qb[["period"]] %in% fStartHorizon : max(qb[["period"]]))
      IEA_WB <- rbind(IEA_WB, qb)
    }
    
    IEA_WB["unit"] <- "Mtoe"
    
    
    names(map_IEA) <- gsub("IEA", "product", names(map_IEA))
    IEA_change_names <- left_join(IEA_WB, map_IEA, by = c("product", "flow"))
    
    unit <- NULL
    IEA_data_WB <- select((IEA_change_names), c(region, period, value, SBS, EF, unit))
    
    names(IEA_data_WB) <- gsub("SBS", "variable", names(IEA_data_WB))
    names(IEA_data_WB) <- gsub("EF", "new", names(IEA_data_WB))
    
    IEA_data_WB <- as.quitte(IEA_data_WB)
    IEA_data_WB <- as.magpie(IEA_data_WB)
    
    map_subsectors_IEA2 <- sets10
    
    # filter to have only the variables which are in enerdata
    map_subsectors_IEA2 <- map_subsectors_IEA2 %>% filter(EF %in% getItems(IEA_data_WB, 3.3))
    
    map_subsectors_IEA <- map_subsectors_by_sector
    
    map_subsectors_IEA[["EF"]] = paste(map_subsectors_IEA[["SBS"]], "Mtoe",map_subsectors_IEA[["EF"]], sep=".")
    map_subsectors_IEA <- map_subsectors_IEA %>% filter(EF %in% getItems(IEA_data_WB, 3))
    
    year <- Reduce(intersect, list(getYears(FCONS_by_sector_MENA, as.integer=TRUE), getYears(IEA_data_WB, as.integer = TRUE)))
    IEA_data_WB <- IEA_data_WB[,year,]
    
    # aggregate from IEA fuels to subsectors
    IEA_by_sector <- toolAggregate(IEA_data_WB[, , as.character(unique(map_subsectors_IEA[["EF"]]))], dim = 3, rel = map_subsectors_IEA, from = "EF", to = "SBS")
    getItems(IEA_by_sector, 3) <- paste0("Final Energy|", sector_name[y],"|", getItems(IEA_by_sector, 3))
    
    # country aggregation
    IEA_by_sector[is.na(IEA_by_sector)] <- 0
    IEA_by_sector <- toolAggregate(IEA_by_sector, rel = rmap)
    
    IEA_by_sector <- as.quitte(IEA_by_sector) %>%
      interpolate_missing_periods(period = getYears(IEA_by_sector,as.integer=TRUE)[1]:getYears(IEA_by_sector,as.integer=TRUE)[length(getYears(IEA_by_sector))], expand.values = TRUE)
    
    IEA_by_sector <- as.quitte(IEA_by_sector) %>% as.magpie()
    years_in_horizon <-  horizon[horizon %in% getYears(IEA_by_sector, as.integer = TRUE)]
    
    IEA_by_sector_GLO <- dimSums(IEA_by_sector, 1)
    getItems(IEA_by_sector_GLO, 1) <- "World"
    IEA_by_sector <- mbind(IEA_by_sector, IEA_by_sector_GLO)
    
    # write data in mif file
    write.report(IEA_by_sector[, years_in_horizon, ], file = "reporting.mif", model = "IEA_WB", unit = "Mtoe", append = TRUE, scenario = "Validation")
    
    #Final Energy IEA
    FE_IEA <- dimSums(IEA_by_sector, dim = 3, na.rm = TRUE)
    getItems(FE_IEA, 3) <- paste0("Final Energy|", sector_name[y])
    
    FE_IEA <- as.quitte(FE_IEA) %>%
      interpolate_missing_periods(period = getYears(FE_IEA,as.integer=TRUE)[1]:getYears(FE_IEA,as.integer=TRUE)[length(getYears(FE_IEA))], expand.values = TRUE)
    
    FE_IEA <- as.quitte(FE_IEA) %>% as.magpie()
    years_in_horizon <-  horizon[horizon %in% getYears(FE_IEA, as.integer = TRUE)]
    
    # write data in mif file
    write.report(FE_IEA[, years_in_horizon, ], file = "reporting.mif", model = "IEA_WB", unit="Mtoe", append = TRUE, scenario = "Validation")
    
    # per fuel
    FCONS_per_fuel_IEA <- IEA_data_WB[,,sets6[sets6[, 1] %in% getItems(IEA_data_WB,3.1),1]]
    
    # remove . from magpie object and replace with |
    FCONS_per_fuel_IEA <- as.quitte(FCONS_per_fuel_IEA)
    FCONS_per_fuel_IEA[[names(FCONS_per_fuel_IEA[, 4])]] <- paste0(FCONS_per_fuel_IEA[[names(FCONS_per_fuel_IEA[, 4])]], "|", FCONS_per_fuel_IEA[["new"]])
    FCONS_per_fuel_IEA <- select(FCONS_per_fuel_IEA, -c("new","unit"))
    FCONS_per_fuel_IEA <- as.quitte(FCONS_per_fuel_IEA) %>% as.magpie()
    getItems(FCONS_per_fuel_IEA, 3) <- paste0("Final Energy|", sector_name[y],"|", getItems(FCONS_per_fuel_IEA, 3))
    
    FCONS_per_fuel_IEA <- as.quitte(FCONS_per_fuel_IEA) %>%
      interpolate_missing_periods(period = getYears(FCONS_per_fuel_IEA,as.integer=TRUE)[1]:getYears(FCONS_per_fuel_IEA,as.integer=TRUE)[length(getYears(FCONS_per_fuel_IEA))], expand.values = TRUE)
    
    FCONS_per_fuel_IEA <- as.quitte(FCONS_per_fuel_IEA) %>% as.magpie()
    years_in_horizon <-  horizon[horizon %in% getYears(FCONS_per_fuel_IEA, as.integer = TRUE)]
    
    FCONS_per_fuel_IEA <- toolAggregate(FCONS_per_fuel_IEA, rel = rmap)
    
    FCONS_per_fuel_IEA_GLO <- dimSums(FCONS_per_fuel_IEA, 1)
    getItems(FCONS_per_fuel_IEA_GLO, 1) <- "World"
    FCONS_per_fuel_IEA <- mbind(FCONS_per_fuel_IEA, FCONS_per_fuel_IEA_GLO)
    
    # write data in mif file
    write.report(FCONS_per_fuel_IEA[,years_in_horizon,],file="reporting.mif",model = "IEA_WB", unit = "Mtoe", append = TRUE, scenario = "Validation")
    
    # Aggregate model IEA by subsector and by energy form
    IEA_by_EF_and_sector <- toolAggregate(IEA_data_WB[, year, as.character(unique(map_subsectors_IEA2[["EF"]]))], dim = 3.3, rel = map_subsectors_IEA2, from = "EF", to = "EFA")
    
    # IEA by subsector and by energy form
    IEA_by_subsector_by_energy_form <- IEA_by_EF_and_sector
    IEA_by_subsector_by_energy_form <- dimSums(IEA_by_subsector_by_energy_form, 3.2, na.rm = TRUE)
    getItems(IEA_by_subsector_by_energy_form, 3.1) <- paste0("Final Energy|", sector_name[y], "|", getItems(IEA_by_subsector_by_energy_form, 3.1))
    
    # country aggregation
    IEA_by_subsector_by_energy_form[is.na(IEA_by_subsector_by_energy_form)] <- 0
    IEA_by_subsector_by_energy_form <- toolAggregate(IEA_by_subsector_by_energy_form, rel = rmap)
    
    # remove . from magpie object
    IEA_by_subsector_by_energy_form <- as.quitte(IEA_by_subsector_by_energy_form)
    IEA_by_subsector_by_energy_form[["variable"]] <- paste0(IEA_by_subsector_by_energy_form[["variable"]], "|", IEA_by_subsector_by_energy_form[["new"]])
    IEA_by_subsector_by_energy_form <- select(IEA_by_subsector_by_energy_form, -c("new"))
    IEA_by_subsector_by_energy_form <- as.quitte(IEA_by_subsector_by_energy_form) %>% as.magpie()
    
    IEA_by_subsector_by_energy_form <- as.quitte(IEA_by_subsector_by_energy_form) %>%
      interpolate_missing_periods(period = getYears(IEA_by_subsector_by_energy_form,as.integer=TRUE)[1]:getYears(IEA_by_subsector_by_energy_form,as.integer=TRUE)[length(getYears(IEA_by_subsector_by_energy_form))], expand.values = TRUE)
    
    IEA_by_subsector_by_energy_form <- as.quitte(IEA_by_subsector_by_energy_form) %>% as.magpie()
    years_in_horizon <-  horizon[horizon %in% getYears(IEA_by_subsector_by_energy_form, as.integer = TRUE)]
    
    IEA_by_subsector_by_energy_form_GLO <- dimSums(IEA_by_subsector_by_energy_form, 1)
    getItems(IEA_by_subsector_by_energy_form_GLO, 1) <- "World"
    IEA_by_subsector_by_energy_form <- mbind(IEA_by_subsector_by_energy_form, IEA_by_subsector_by_energy_form_GLO)
    
    # write data in mif file
    write.report(IEA_by_subsector_by_energy_form[, years_in_horizon, ], file = "reporting.mif", model = "IEA_WB", unit = "Mtoe", append = TRUE, scenario = "Validation")
    
    # Aggregate model IEA by energy form
    IEA_by_energy_form <- dimSums(IEA_by_EF_and_sector, 3.1, na.rm = TRUE)
    getItems(IEA_by_energy_form,3) <- paste0("Final Energy|", sector_name[y], "|", getItems(IEA_by_energy_form, 3.2))
    
    # country aggregation
    IEA_by_energy_form[is.na(IEA_by_energy_form)] <- 0
    IEA_by_energy_form <- toolAggregate(IEA_by_energy_form, rel = rmap)
    
    IEA_by_energy_form <- as.quitte(IEA_by_energy_form) %>%
      interpolate_missing_periods(period = getYears(IEA_by_energy_form,as.integer=TRUE)[1]:getYears(IEA_by_energy_form,as.integer=TRUE)[length(getYears(IEA_by_energy_form))], expand.values = TRUE)
    
    IEA_by_energy_form <- as.quitte(IEA_by_energy_form) %>% as.magpie()
    years_in_horizon <-  horizon[horizon %in% getYears(IEA_by_energy_form, as.integer = TRUE)]
    
    IEA_by_energy_form_GLO <- dimSums(IEA_by_energy_form, 1)
    getItems(IEA_by_energy_form_GLO, 1) <- "World"
    IEA_by_energy_form <- mbind(IEA_by_energy_form, IEA_by_energy_form_GLO)
    
    # write data in mif file
    write.report(IEA_by_energy_form[, years_in_horizon, ], file = "reporting.mif", model = "IEA_WB", unit = "Mtoe", append = TRUE, scenario = "Validation")
    
    #############    Final Energy consumption from Navigate
    # load current OPENPROM set configuration
    sets_Navigate <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), sector[y])
    sets_Navigate <- unlist(strsplit(sets_Navigate[, 1], ","))
    
    # use navigate-openprom mapping to extract correct data from source
    map_Navigate <- toolGetMapping(name = "prom-navigate-fucon-mapping.csv",
                                   type = "sectoral",
                                   where = "mrprom")
    maps <- map_Navigate
    
    # filter mapping to keep only XXX sectors
    map_Navigate <- filter(map_Navigate, map_Navigate[, "SBS"] %in% sets_Navigate)
    
    # ..and only items that have an Navigate-prom mapping
    Navigate <- map_Navigate[!is.na(map_Navigate[, "Navigate"]), "Navigate"]
    map_Navigate <- map_Navigate[map_Navigate[, "Navigate"] %in% Navigate, ]
    
    # remove the empty cells from mapping
    map_Navigate <- map_Navigate[!(map_Navigate[, "Navigate"] == ""), ]
    
    # filter navigate data by scenario different for each sector
    if (sector[y] %in% c("DOMSE", "NENSE")) {
      
      world_Navigate_NPi <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = FALSE)
      world_Navigate_NPi <- world_Navigate_NPi["World",,]
     
      x1 <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = TRUE)
      x1 <- x1[,,unique(map_Navigate[map_Navigate[,"Navigate"] %in% getItems(x1,3.3), 6])]
      world_Navigate <- world_Navigate_NPi[,,unique(map_Navigate[map_Navigate[,"Navigate"] %in% getItems(world_Navigate_NPi,3.3), 6])]
      
      world_Navigate <- world_Navigate[, Reduce(intersect, list(getYears(world_Navigate), getYears(x1))), ]
      x1 <- x1[, Reduce(intersect, list(getYears(world_Navigate), getYears(x1))), ]
      
      x1 <- mbind(world_Navigate, x1)
      
      world_Navigate_Dem <- readSource("Navigate", subtype = "NAV_Dem-NPi-ref", convert = FALSE)
      world_Navigate_Dem <- world_Navigate_Dem["World",,]
      
      x2 <- readSource("Navigate", subtype = "NAV_Dem-NPi-ref", convert = TRUE)
      x2 <- x2[,,unique(map_Navigate[map_Navigate[,"Navigate"] %in% getItems(x2,3.3), 6])]
      
      world_Navigate <- world_Navigate_Dem[,,unique(map_Navigate[map_Navigate[,"Navigate"] %in% getItems(world_Navigate_Dem,3.3), 6])]
      items <- !(getItems(world_Navigate, 3) %in% getItems(x2, 3))
      x2 <- add_columns(x2, addnm = getItems(world_Navigate[,,items], 3), dim = 3, fill = NA)
      
      world_Navigate <- world_Navigate[, Reduce(intersect, list(getYears(world_Navigate), getYears(x2))), ]
      x2 <- x2[, Reduce(intersect, list(getYears(world_Navigate), getYears(x2))), ]
      x2 <- mbind(world_Navigate[,,], x2[,,])
      
      #keep common years that exist in the scenarios
      years <- intersect(getYears(x1,as.integer=TRUE), getYears(x2, as.integer = TRUE))
      x <- mbind(x1[, years,], x2[, years, ])
    }
    
    # for TRANSE use of NAV_Ind_NPi because it has truck data
    if (sector[y] %in% c("INDSE", "TRANSE")) {
      world_Navigate_NPi <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = FALSE)
      world_Navigate_NPi <- world_Navigate_NPi["World",,]
      
      x1 <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = TRUE)
      x1 <- x1[,,unique(map_Navigate[map_Navigate[,"Navigate"] %in% getItems(x1,3.3), 6])]
      world_Navigate <- world_Navigate_NPi[,,unique(map_Navigate[map_Navigate[,"Navigate"] %in% getItems(world_Navigate_NPi,3.3), 6])]
      
      world_Navigate <- world_Navigate[, Reduce(intersect, list(getYears(world_Navigate), getYears(x1))), ]
      x1 <- x1[, Reduce(intersect, list(getYears(world_Navigate), getYears(x1))), ]
      
      x1 <- mbind(world_Navigate, x1)
      
      world_Navigate_Ind <- readSource("Navigate", subtype = "NAV_Ind_NPi", convert = FALSE)
      world_Navigate_Ind <- world_Navigate_Ind["World",,]
      
      x2 <- readSource("Navigate", subtype = "NAV_Ind_NPi", convert = TRUE)
      x2 <- x2[,,unique(map_Navigate[map_Navigate[,"Navigate"] %in% getItems(x2,3.3), 6])]
      
      world_Navigate <- world_Navigate_Ind[,,unique(map_Navigate[map_Navigate[,"Navigate"] %in% getItems(world_Navigate_Ind,3.3), 6])]
      items <- !(getItems(world_Navigate, 3) %in% getItems(x2, 3))
      x2 <- add_columns(x2, addnm = getItems(world_Navigate[,,items], 3), dim = 3, fill = NA)
      
      world_Navigate <- world_Navigate[, Reduce(intersect, list(getYears(world_Navigate), getYears(x2))), ]
      x2 <- x2[, Reduce(intersect, list(getYears(world_Navigate), getYears(x2))), ]
      x2 <- mbind(world_Navigate[,,], x2[,,])
      
      #keep common years that exist in the scenarios
      years <- intersect(getYears(x1,as.integer=TRUE), getYears(x2, as.integer = TRUE))
      x <- mbind(x1[, years,], x2[, years, ])
    }
    
    # filter data to keep only Navigate variables
    x <- x[, , map_Navigate[, "Navigate"]]
    
    # EJ to Mtoe
    x <- x * 23.8846
    getItems(x, 3.4) <- "Mtoe"
    x <- as.quitte(x)
    value.x <- NULL
    value.y <- NULL
    value <- NULL
    
    # if SUP_NPi_Default has NA take the value of the second scenario
    x <- full_join(x[which(x[,2] == "SUP_NPi_Default"),], x[which(x[,2] != "SUP_NPi_Default"),], by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    
    
    # rename variables from Navigate to openprom names
    names(map_Navigate) <- gsub("Navigate", "variable", names(map_Navigate))
    x <- left_join(x, map_Navigate[,  c(2,3,6)], by = "variable")
    
    # drop variable names of navigate
    x <- select(x, -c("variable"))
    
    # rename columns of data
    names(x) <- gsub("SBS", "variable", names(x))
    names(x) <- gsub("EF", "new", names(x))
    
    x <- as.quitte(x)
    x <- as.magpie(x)
    
    #add dimensions, GDO 75% of LQD and GSL 25% of LQD
    # x <- add_columns(x, addnm = c("GDO"), dim = "new", fill = 0.75)
    # x2 <- add_columns(x, addnm = c("LQD"), dim = "new", fill = 0)
    # x2 <- x2[,,getItems(x2[,,"LQD"],3)[!(getItems(x2[,,"LQD"],3) %in% getItems(x[,,"LQD"],3))]]
    # x <- mbind(x, x2)
    # x[,,"GDO"] <- x[,,"LQD"] * x[,,"GDO"]
    # 
    # x <- add_columns(x, addnm = c("GSL"), dim = "new", fill = 0.25)
    # y2 <- add_columns(x, addnm = c("LQD"), dim = "new", fill = 0)
    # y2 <- y2[,,getItems(y2[,,"LQD"],3)[!(getItems(y2[,,"LQD"],3) %in% getItems(x[,,"LQD"],3))]]
    # x <- mbind(x, y2)
    # x[,,"GSL"] <- x[,,"LQD"] * x[,,"GSL"]
    
    if (sector[y] == "INDSE") {
      #OI is FE total per fuel - the sum of the other subsectors per fuel
      sum_subsectors <- dimSums(x[,,getItems(x,3.3)[!(getItems(x,3.3) %in% "OI")]][,,getItems(x[,,"OI"],3.5)], dim = 3.3, na.rm = TRUE)
      sum_subsectors <- as.quitte(sum_subsectors)
      sum_subsectors["variable"] <- "OI"
      sum_subsectors <- sum_subsectors[, c(1, 2, 3, 4, 8 , 5 , 6 , 7)]
      sum_subsectors <- as.quitte(sum_subsectors)
      sum_subsectors <- as.magpie(sum_subsectors)
      items <- intersect(getItems(x, 3), getItems(sum_subsectors, 3))
      x[,,"OI"][,,items] <- x[,,"OI"][,,items] - ifelse(is.na(sum_subsectors[,,items]), 0, sum_subsectors[,,items])
      x[x < 0] <- 10^-6
      x[,,"OI"][,,"NGS"][x[,,"OI"][,,"NGS"] == 0] <- 10^-6
    }
    
    if (sector[y] == "TRANSE") {
      
      a1 <- readSource("IRF", subtype = "inland-surface-passenger-transport-by-rail")
      #million pKm/yr
      a2 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-rail")
      #million tKm/yr
      a1 <- a1[, Reduce(intersect, list(getYears(a1), getYears(a2))), ]
      a2 <- a2[, Reduce(intersect, list(getYears(a1), getYears(a2))), ]
      out1 <- (a1 / (a1 + a2))
      out1 <- ifelse(is.na(out1), mean(out1, na.rm=TRUE), out1)
      out1 <- as.quitte(out1)
      out1 <- mutate(out1, value = mean(value, na.rm = TRUE), .by = c("region"))
      out1 <- select(out1, c("region", "value"))
      out1 <- distinct(out1)
      out1 <- as.quitte(out1) %>% as.magpie()
      out1 <- add_columns(out1, addnm = c("World"), dim = 1, fill = mean(out1))
      x[,,"PT"] <- x[,,"PT"] * out1
      
      out3 <- (a2 / (a1 + a2))
      out3 <- ifelse(is.na(out3), mean(out3, na.rm=TRUE), out3)
      out3 <- as.quitte(out3)
      out3 <- mutate(out3, value = mean(value, na.rm = TRUE), .by = c("region"))
      out3 <- select(out3, c("region", "value"))
      out3 <- distinct(out3)
      out3 <- as.quitte(out3) %>% as.magpie()
      out3 <- add_columns(out3, addnm = c("World"), dim = 1, fill = mean(out3))
      x[,,"GT"] <- x[,,"GT"] * out3
      
      a3 <- readSource("IRF", subtype = "inland-surface-private-passenger-transport-by-road")
      #million pKm/yr
      a4 <- readSource("IRF", subtype = "inland-surface-passenger-transport-total")
      #million pKm/yr
      a3 <- a3[, Reduce(intersect, list(getYears(a3), getYears(a4))), ]
      a4 <- a4[, Reduce(intersect, list(getYears(a3), getYears(a4))), ]
      out2 <- (a3 / (a4))
      out2 <- ifelse(is.na(out2), mean(out2, na.rm=TRUE), out2)
      out2 <- as.quitte(out2)
      out2 <- mutate(out2, value = mean(value, na.rm = TRUE), .by = c("region"))
      out2 <- select(out2, c("region", "value"))
      out2 <- distinct(out2)
      out2 <- as.quitte(out2) %>% as.magpie()
      out2 <- add_columns(out2, addnm = c("World"), dim = 1, fill = mean(out2))
      x[,,"PC"] <- x[,,"PC"] * out2
      
      a5 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-inland-waterway")
      #million tKm/yr
      a6 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-rail")
      #million tKm/yr
      a7 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-road")
      #million tKm/yr
      
      a5 <- a5[, Reduce(intersect, list(getYears(a5), getYears(a6), getYears(a7))), ]
      a6 <- a6[, Reduce(intersect, list(getYears(a5), getYears(a6), getYears(a7))), ]
      a7 <- a7[, Reduce(intersect, list(getYears(a5), getYears(a6), getYears(a7))), ]
      
      out4 <- (a5 / (a5 + a6 + a7))
      out4 <- ifelse(is.na(out4), mean(out4, na.rm=TRUE), out4)
      out4 <- as.quitte(out4)
      out4 <- mutate(out4, value = mean(value, na.rm = TRUE), .by = c("region"))
      out4 <- select(out4, c("region", "value"))
      out4 <- distinct(out4)
      out4 <- as.quitte(out4) %>% as.magpie()
      out4 <- add_columns(out4, addnm = c("World"), dim = 1, fill = mean(out4))
      x[,,"GN"] <- x[,,"GN"] * out4
      
      # # remove GSL from PT & GT in iFuelConsTRANSE
      # x[,,"PT"][,,"GSL"] <- 10^-6
      # x[,,"GT"][,,"GSL"] <- 10^-6
      # 
      # # We want 100% of liquids to be GDO for GT & PT
      # 
      # x[,,"PT"][,,"GDO"] <- x[,,"PT"][,,"GDO"] / 0.75
      # x[,,"GT"][,,"GDO"] <- x[,,"GT"][,,"GDO"] / 0.75
      
    }
    
    # set NA to 0
    x[is.na(x)] <- 10^-6
    
    map_subsectors_Navigate2 <- sets10
    
    # filter to have only the variables which are in enerdata
    map_subsectors_Navigate2 <- map_subsectors_Navigate2 %>% filter(EF %in% getItems(x, 3.5))
    x <- as.quitte(x)
    x <- as.magpie(x)
    
    x <- x[, getYears(x, as.integer = T) %in% c(fStartHorizon : 2100), ]
    year <- getYears(x)
    
    Navigate_by_sector <- dimSums(x, dim = 3.5)
    
    getItems(Navigate_by_sector, 3.3) <- paste0("Final Energy|", sector_name[y], "|", getItems(Navigate_by_sector, 3.3))
    
    # country aggregation
    Navigate_by_sector[is.na(Navigate_by_sector)] <- 0
    
    Navigate_by_sector_world <- Navigate_by_sector["World",,]
    Navigate_by_sector <- Navigate_by_sector[as.character(getISOlist()), , ]
    Navigate_by_sector <- toolAggregate(Navigate_by_sector, rel = rmap)
    Navigate_by_sector <- mbind(Navigate_by_sector, Navigate_by_sector_world)
    
    # write data in mif file, sector INDSE, works without aggregation in the next step
    if (!(sector[y] %in% c("INDSE"))) {
      Navigate_by_sector <- as.quitte(Navigate_by_sector) %>%
        interpolate_missing_periods(period = getYears(Navigate_by_sector,as.integer=TRUE)[1]:getYears(Navigate_by_sector,as.integer=TRUE)[length(getYears(Navigate_by_sector))], expand.values = TRUE)
      
      Navigate_by_sector <- as.quitte(Navigate_by_sector) %>% as.magpie()
      years_in_horizon <-  horizon[horizon %in% getYears(Navigate_by_sector, as.integer = TRUE)]
      
      write.report(Navigate_by_sector[, years_in_horizon, ], file = "reporting.mif", append = TRUE)
    }
    
    # per fuel
    FCONS_per_fuel_Navigate <- x[,,sets6[sets6[, 1] %in% getItems(x,3.3),1]]
    
    # remove . from magpie object and replace with |
    FCONS_per_fuel_Navigate <- as.quitte(FCONS_per_fuel_Navigate)
    FCONS_per_fuel_Navigate[[names(FCONS_per_fuel_Navigate[, 4])]] <- paste0(FCONS_per_fuel_Navigate[[names(FCONS_per_fuel_Navigate[, 4])]], "|", FCONS_per_fuel_Navigate[["new"]])
    FCONS_per_fuel_Navigate <- select(FCONS_per_fuel_Navigate, -c("new"))
    FCONS_per_fuel_Navigate <- as.quitte(FCONS_per_fuel_Navigate) %>% as.magpie()
    getItems(FCONS_per_fuel_Navigate, 3.3) <- paste0("Final Energy|", sector_name[y],"|", getItems(FCONS_per_fuel_Navigate, 3.3))
    
    FCONS_per_fuel_Navigate <- as.quitte(FCONS_per_fuel_Navigate) %>%
      interpolate_missing_periods(period = getYears(FCONS_per_fuel_Navigate,as.integer=TRUE)[1]:getYears(FCONS_per_fuel_Navigate,as.integer=TRUE)[length(getYears(FCONS_per_fuel_Navigate))], expand.values = TRUE)
    
    FCONS_per_fuel_Navigate <- as.quitte(FCONS_per_fuel_Navigate) %>% as.magpie()
    years_in_horizon <-  horizon[horizon %in% getYears(FCONS_per_fuel_Navigate, as.integer = TRUE)]
    
    FCONS_per_fuel_Navigate_world <- FCONS_per_fuel_Navigate["World",,]
    FCONS_per_fuel_Navigate <- FCONS_per_fuel_Navigate[as.character(getISOlist()), , ]
    FCONS_per_fuel_Navigate <- toolAggregate(FCONS_per_fuel_Navigate, rel = rmap)
    FCONS_per_fuel_Navigate <- mbind(FCONS_per_fuel_Navigate, FCONS_per_fuel_Navigate_world)
    
    # write data in mif file
    write.report(FCONS_per_fuel_Navigate[,years_in_horizon,],file="reporting.mif", append = TRUE)
    
    # Aggregate model Navigate by subsector and by energy form
    Navigate_by_EF_and_sector <- toolAggregate(x[, , as.character(unique(map_subsectors_Navigate2[["EF"]]))], dim = c(3.5), rel = map_subsectors_Navigate2, from = "EF", to = "EFA")
    
    # Aggregate model Navigate by energy form
    Navigate_by_energy_form6 <- Navigate_by_EF_and_sector
    
    getItems(Navigate_by_energy_form6,3.3) <- paste0("Final Energy|", sector_name[y], "|", getItems(Navigate_by_energy_form6, 3.3))
    
    # country aggregation
    Navigate_by_energy_form6[is.na(Navigate_by_energy_form6)] <- 0
    
    Navigate_by_energy_form6_world <- Navigate_by_energy_form6["World",,]
    Navigate_by_energy_form6 <- Navigate_by_energy_form6[as.character(getISOlist()), , ]
    Navigate_by_energy_form6 <- toolAggregate(Navigate_by_energy_form6, rel = rmap)
    Navigate_by_energy_form6 <- mbind(Navigate_by_energy_form6, Navigate_by_energy_form6_world)
    
    # remove . from magpie object
    Navigate_by_energy_form6 <- as.quitte(Navigate_by_energy_form6)
    Navigate_by_energy_form6[["variable"]] <- paste0(Navigate_by_energy_form6[["variable"]], "|", Navigate_by_energy_form6[["new"]])
    Navigate_by_energy_form6 <- select(Navigate_by_energy_form6, -c("new"))
    Navigate_by_energy_form6 <- as.quitte(Navigate_by_energy_form6) %>% as.magpie()
    
    Navigate_by_energy_form6 <- as.quitte(Navigate_by_energy_form6) %>%
      interpolate_missing_periods(period = getYears(Navigate_by_energy_form6,as.integer=TRUE)[1]:getYears(Navigate_by_energy_form6,as.integer=TRUE)[length(getYears(Navigate_by_energy_form6))], expand.values = TRUE)
    
    Navigate_by_energy_form6 <- as.quitte(Navigate_by_energy_form6) %>% as.magpie()
    years_in_horizon <-  horizon[horizon %in% getYears(Navigate_by_energy_form6, as.integer = TRUE)]
    
    # write data in mif file
    write.report(Navigate_by_energy_form6[, years_in_horizon, ], file = "reporting.mif", append = TRUE)
    
  }
  
  # Add IEA Total
  
  map_IEA_Total <- toolGetMapping(name = "IEA-by-fuel.csv",
                                  type = "sectoral",
                                  where = "mrprom")
  
  map_IEA_Total <- map_IEA_Total %>% drop_na("product")
  map_IEA_Total <- map_IEA_Total %>% select(-"X")
  IEA_Total <- NULL
  for (ii in unique(map_IEA_Total[, "flow"])) {
    d <- readSource("IEA", subtype = as.character(ii))
    d <- d / 1000 #ktoe to mtoe
    d <- as.quitte(d)
    
    # each flow has some products as it is the EF column of map
    m <- filter(map_IEA_Total, map_IEA_Total[["flow"]] == ii)
    
    # for each product of IEA data
    region <- NULL
    period <- NULL
    
    qb <- filter(d, product %in% m[["product"]])
    qb <- select((qb), c(region, period, value, product, flow))
    
    qb <- filter(qb, qb[["period"]] %in% fStartHorizon : max(qb[["period"]]))
    IEA_Total <- rbind(IEA_Total, qb)
  }
  
  magpie_IEA_Total <- as.quitte(IEA_Total) %>% as.magpie()
  
  # choose years that both models have
  year <- Reduce(intersect, list(getYears(MENA_EDS_VFeCons, as.integer = TRUE), getYears(magpie_IEA_Total, as.integer = TRUE)))
  
  consumption_IEA_variables <- as.quitte(IEA_Total)
  
  # add a column with the fuels that match each variable of enerdata
  IEA_Balances_Total <- left_join(consumption_IEA_variables, map_IEA_Total, by = c("product", "flow"))
  IEA_Balances_Total["variable"] <- paste0("Final Energy|", IEA_Balances_Total[["fuel"]])
  IEA_Balances_Total["unit"] <- "Mtoe"
  IEA_Balances_Total <- IEA_Balances_Total[, c(1, 2, 3, 4, 5, 6, 9)]
  IEA_Balances_Total <- filter(IEA_Balances_Total, period %in% year)
  IEA_Balances_Total <- as.quitte(IEA_Balances_Total)
  IEA_Balances_Total <- as.magpie(IEA_Balances_Total)
  IEA_Balances_Total[is.na(IEA_Balances_Total)] <- 0
  IEA_Balances_Total <- toolAggregate(IEA_Balances_Total, rel = rmap)
  
  l <- getNames(IEA_Balances_Total) == "Final Energy|Total.Mtoe"
  getNames(IEA_Balances_Total)[l] <- "Final Energy.Mtoe"
  
  IEA_Balances_Total <- as.quitte(IEA_Balances_Total) %>%
    interpolate_missing_periods(period = getYears(IEA_Balances_Total,as.integer=TRUE)[1]:getYears(IEA_Balances_Total,as.integer=TRUE)[length(getYears(IEA_Balances_Total))], expand.values = TRUE)
  
  IEA_Balances_Total <- as.quitte(IEA_Balances_Total) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(IEA_Balances_Total, as.integer = TRUE)]
  
  IEA_Balances_Total_GLO <- dimSums(IEA_Balances_Total, 1)
  getItems(IEA_Balances_Total_GLO, 1) <- "World"
  IEA_Balances_Total <- mbind(IEA_Balances_Total, IEA_Balances_Total_GLO)
  
  # write data in mif file
  write.report(IEA_Balances_Total[,years_in_horizon , ], file = "reporting.mif", model = "IEA_Total", unit = "Mtoe", append = TRUE, scenario = "Validation")
  
  ########## Add Final Energy total to reporting Navigate
  
  # Map Final Energy Navigate
  map_Navigate_Total <- toolGetMapping(name = "Navigate-by-fuel.csv",
                                       type = "sectoral",
                                       where = "mrprom")
  
  map_Navigate_Total <- map_Navigate_Total %>% drop_na("Navigate")
  

  world_Navigate_NPi_total <- world_Navigate_NPi[,,unique(map_Navigate_Total[map_Navigate_Total[,"Navigate"] %in% getItems(world_Navigate_NPi,3.3), 2])]
  world_Navigate_Dem_total <- world_Navigate_Dem[,,unique(map_Navigate_Total[map_Navigate_Total[,"Navigate"] %in% getItems(world_Navigate_Dem,3.3), 2])]
  world_Navigate_Ind_total <- world_Navigate_Ind[,,unique(map_Navigate_Total[map_Navigate_Total[,"Navigate"] %in% getItems(world_Navigate_Ind,3.3), 2])]
  
  # filter Navigate by scenarios
  x1 <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = TRUE)
  x1 <- x1[,,unique(map_Navigate_Total[map_Navigate_Total[,"Navigate"] %in% getItems(x1,3.3), 2])]
  years <- intersect(getYears(x1,as.integer=TRUE), getYears(world_Navigate_NPi_total, as.integer = TRUE))
  x1 <- mbind(x1[,years,], world_Navigate_NPi_total[,years,])
  x2 <- readSource("Navigate", subtype = "NAV_Dem-NPi-ref", convert = TRUE)
  x2 <- x2[,,unique(map_Navigate_Total[map_Navigate_Total[,"Navigate"] %in% getItems(world_Navigate_Dem_total,3.3), 2])]
  years <- intersect(getYears(x2,as.integer=TRUE), getYears(world_Navigate_Dem_total, as.integer = TRUE))
  items <- !(getItems(world_Navigate_Dem_total, 3) %in% getItems(x2, 3))
  x2 <- add_columns(x2, addnm = getItems(world_Navigate_Dem_total[,,items], 3), dim = 3, fill = NA)
  x2 <- mbind(x2[,years,], world_Navigate_Dem_total[,years,])
  x3 <- readSource("Navigate", subtype = "NAV_Ind_NPi", convert = TRUE)
  x3 <- x3[,,unique(map_Navigate_Total[map_Navigate_Total[,"Navigate"] %in% getItems(x3,3.3), 2])]
  years <- intersect(getYears(x3,as.integer=TRUE), getYears(world_Navigate_Ind_total, as.integer = TRUE))
  items <- !(getItems(world_Navigate_Ind_total, 3) %in% getItems(x3, 3))
  x3 <- add_columns(x3, addnm = getItems(world_Navigate_Ind_total[,,items], 3), dim = 3, fill = NA)
  x3 <- mbind(x3[,years,], world_Navigate_Ind_total[,years,])
  
  x4 <- readSource("Navigate", subtype = "SUP_1p5C_Default", convert = TRUE)
  x4 <- x4[,,unique(map_Navigate_Total[map_Navigate_Total[,"Navigate"] %in% getItems(x4,3.3), 2])]
  world_Navigate_1p5C <- readSource("Navigate", subtype = "SUP_1p5C_Default", convert = FALSE)
  world_Navigate_1p5C <- world_Navigate_1p5C["World",,]
  world_Navigate_1p5C_total <- world_Navigate_1p5C[,,unique(map_Navigate_Total[map_Navigate_Total[,"Navigate"] %in% getItems(world_Navigate_1p5C,3.3), 2])]
  years <- intersect(getYears(x4,as.integer=TRUE), getYears(world_Navigate_1p5C_total, as.integer = TRUE))
  x4 <- mbind(x4[,years,], world_Navigate_1p5C_total[,years,])
  
  x5 <- readSource("Navigate", subtype = "SUP_2C_Default", convert = TRUE)
  x5 <- x5[,,unique(map_Navigate_Total[map_Navigate_Total[,"Navigate"] %in% getItems(x5,3.3), 2])]
  world_Navigate_2C <- readSource("Navigate", subtype = "SUP_2C_Default", convert = FALSE)
  world_Navigate_2C <- world_Navigate_2C["World",,]
  world_Navigate_2C_total <- world_Navigate_2C[,,unique(map_Navigate_Total[map_Navigate_Total[,"Navigate"] %in% getItems(world_Navigate_2C,3.3), 2])]
  years <- intersect(getYears(x5,as.integer=TRUE), getYears(world_Navigate_2C_total, as.integer = TRUE))
  x5 <- mbind(x5[,years,], world_Navigate_2C_total[,years,])
  
  # keep common years that exist in the scenarios
  x1 <- x1[, Reduce(intersect, list(getYears(x1), getYears(x2), getYears(x3), getYears(x4), getYears(x5))), ]
  x2 <- x2[, Reduce(intersect, list(getYears(x1), getYears(x2), getYears(x3), getYears(x4), getYears(x5))), ]
  x3 <- x3[, Reduce(intersect, list(getYears(x1), getYears(x2), getYears(x3), getYears(x4), getYears(x5))), ]
  x4 <- x4[, Reduce(intersect, list(getYears(x1), getYears(x2), getYears(x3), getYears(x4), getYears(x5))), ]
  x5 <- x5[, Reduce(intersect, list(getYears(x1), getYears(x2), getYears(x3), getYears(x4), getYears(x5))), ]
   
  x <- mbind(x1, x2, x3, x4, x5)
  
  # filter data to keep only Navigate map variables
  navigate_total <- x[,,map_Navigate_Total[,"Navigate"]] * 23.8846 # EJ to Mtoe
  
  # choose years
  x <- x[, getYears(x, as.integer = T) %in% c(fStartHorizon : 2100), ]
  year <- getYears(x)
  
  Navigate_Balances_Total <- as.quitte(navigate_total)
  names(map_Navigate_Total) <- sub("Navigate", "variable", names(map_Navigate_Total))
  
  # add a column with the fuels that match each variable of Navigate
  Navigate_Balances_Total <- left_join(Navigate_Balances_Total, map_Navigate_Total, by = c("variable"))
  
  # drop column variable and rename column fuel
  Navigate_Balances_Total <- select(Navigate_Balances_Total, -"variable")
  names(Navigate_Balances_Total) <- sub("fuel", "variable", names(Navigate_Balances_Total))
  
  # EJ to Mtoe
  Navigate_Balances_Total["unit"] <- "Mtoe"
  
  # choose common years that navigate and OPEN-PROM models have
  Navigate_Balances_Total <- filter(Navigate_Balances_Total, period %in% c(fStartHorizon : 2100))
  
  qNavigate_Balances_Total <- as.quitte(Navigate_Balances_Total)
  
  # take the sum of each subsector(for DOMSE and NENSE)
  qNavigate_Balances_Total <- mutate(qNavigate_Balances_Total, value = sum(value, na.rm = TRUE), .by = c("model", "scenario", "region", "variable", "unit", "period"))
  
  # remove duplicates
  qNavigate_Balances_Total <- distinct(qNavigate_Balances_Total)
  
  qNavigate_Balances_Total <- filter(qNavigate_Balances_Total, !is.na(qNavigate_Balances_Total[["value"]]))
  
  Navigate_Balances_Total <- as.quitte(qNavigate_Balances_Total) %>% as.magpie()
  
  #country aggregation
  Navigate_Balances_Total[is.na(Navigate_Balances_Total)] <- 0
  Navigate_Balances_Total_world <- Navigate_Balances_Total["World",,]
  Navigate_Balances_Total <- Navigate_Balances_Total[as.character(getISOlist()), , ]
  Navigate_Balances_Total <- toolAggregate(Navigate_Balances_Total, rel = rmap)
  Navigate_Balances_Total <- mbind(Navigate_Balances_Total, Navigate_Balances_Total_world)
  
  Navigate_Balances_Total[is.na(Navigate_Balances_Total)] <- 0
  
  Navigate_Balances_Total <- as.quitte(Navigate_Balances_Total) %>%
    interpolate_missing_periods(period = getYears(Navigate_Balances_Total,as.integer=TRUE)[1]:getYears(Navigate_Balances_Total,as.integer=TRUE)[length(getYears(Navigate_Balances_Total))], expand.values = TRUE)
  
  Navigate_Balances_Total <- as.quitte(Navigate_Balances_Total) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(Navigate_Balances_Total, as.integer = TRUE)]
  
  # write data in mif file
  write.report(Navigate_Balances_Total[, years_in_horizon, ], file = "reporting.mif", append = TRUE)
  
  
  ######### reportEmissions
  
  # Link between Model Subsectors and Fuels
  sets4 <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "SECTTECH")
  sets4[6,] <- paste0(sets4[6,] , sets4[7,])
  sets4 <- sets4[ - c(7),,drop = FALSE]
  sets4[7,] <- paste0(sets4[7,] , sets4[8,], sets4[9,])
  sets4 <- sets4[ - c(8, 9),,drop = FALSE]
  sets4 <- separate_wider_delim(sets4,cols = 1, delim = ".", names = c("SBS","EF"))
  sets4[["EF"]] <- sub("\\(","",sets4[["EF"]])
  sets4[["EF"]] <- sub("\\)","",sets4[["EF"]])
  sets4[["SBS"]] <- sub("\\(","",sets4[["SBS"]])
  sets4[["SBS"]] <- sub("\\)","",sets4[["SBS"]])
  sets4 <- separate_rows(sets4,EF)
  sets4 <- separate_rows(sets4,SBS)
  sets4 <- filter(sets4, EF != "")
  
  EFtoEFS <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "EFtoEFS")
  EFtoEFS <- as.data.frame(EFtoEFS)
  EFtoEFS <- separate_wider_delim(EFtoEFS,cols = 1, delim = ".", names = c("EF","EFS"))
  EFtoEFS[["EF"]] <- sub("\\(","",EFtoEFS[["EF"]])
  EFtoEFS[["EF"]] <- sub("\\)","",EFtoEFS[["EF"]])
  EFS <- NULL
  EFtoEFS <- EFtoEFS %>% separate_longer_delim(c(EF, EFS), delim = ",")
  
  IND <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "INDDOM")
  IND <- unlist(strsplit(IND[, 1], ","))
  IND <- as.data.frame(IND)
  
  map_INDDOM <- sets4 %>% filter(SBS %in% IND[,1])
  
  map_INDDOM <- filter(map_INDDOM, EF != "")
  
  qINDDOM <- left_join(map_INDDOM, EFtoEFS, by = "EF")
  qINDDOM <- select((qINDDOM), -c(EF))
  
  qINDDOM <- unique(qINDDOM)
  names(qINDDOM) <- sub("EFS", "SECTTECH", names(qINDDOM))
  
  qINDDOM <- paste0(qINDDOM[["SBS"]], ".", qINDDOM[["SECTTECH"]])
  INDDOM <- as.data.frame(qINDDOM)
  
  PGEF <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "PGEF")
  PGEF <- as.data.frame(PGEF)
  
  TRANSE <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "TRANSE")
  TRANSE <- unlist(strsplit(TRANSE[, 1], ","))
  TRANSE <- as.data.frame(TRANSE)
  
  map_TRANSECTOR <- sets4 %>% filter(SBS %in% TRANSE[,1])
  map_TRANSECTOR <- paste0(map_TRANSECTOR[["SBS"]], ".", map_TRANSECTOR[["EF"]])
  map_TRANSECTOR <- as.data.frame(map_TRANSECTOR)
  
  PGALL <- NULL
  PGALLtoEF <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "PGALLtoEF")
  PGALLtoEF <- as.data.frame(PGALLtoEF)
  PGALLtoEF <- separate_wider_delim(PGALLtoEF, cols = 1, delim = ".", names = c("PGALL","EF"))
  PGALLtoEF[["PGALL"]] <- sub("\\(","",PGALLtoEF[["PGALL"]])
  PGALLtoEF[["PGALL"]] <- sub("\\)","",PGALLtoEF[["PGALL"]])
  PGALLtoEF <- separate_rows(PGALLtoEF, PGALL)
  
  CCS <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "CCS")
  CCS <- as.data.frame(CCS)
  
  CCS <- PGALLtoEF[PGALLtoEF[["PGALL"]] %in% CCS[["CCS"]], ]
  
  SECTTECH2 <- sets4 %>% filter(SBS %in% c("BU"))
  SECTTECH2 <- paste0(SECTTECH2[["SBS"]], ".", SECTTECH2[["EF"]])
  SECTTECH2 <- as.data.frame(SECTTECH2)
  
  # add model MENA_EDS data (choosing the correct variable from MENA by use of the MENA-PROM mapping)
  MENA_iCo2EmiFac <- readSource("MENA_EDS", subtype =  map[map[["OPEN.PROM"]] == "iCo2EmiFac", "MENA.EDS"])
  MENA_VConsFuel <- readSource("MENA_EDS", subtype =  map[map[["OPEN.PROM"]] == "VConsFuel", "MENA.EDS"])
  MENA_VTransfInThermPowPls <- readSource("MENA_EDS", subtype =  map[map[["OPEN.PROM"]] == "VInpTransfTherm", "MENA.EDS"])
  MENA_VTransfInputDHPlants <- readSource("MENA_EDS", subtype =  map[map[["OPEN.PROM"]] == "VTransfInputDHPlants", "MENA.EDS"])
  MENA_VEnCons <- readSource("MENA_EDS", subtype =  map[map[["OPEN.PROM"]] == "VConsFiEneSec", "MENA.EDS"])
  MENA_VDemTr <- readSource("MENA_EDS", subtype =  map[map[["OPEN.PROM"]] == "VDemFinEneTranspPerFuel", "MENA.EDS"])
  MENA_VElecProd <- readSource("MENA_EDS", subtype =  map[map[["OPEN.PROM"]] == "VProdElec", "MENA.EDS"])
  MENA_iPlantEffByType <- readSource("MENA_EDS", subtype =  map[map[["OPEN.PROM"]] == "iPlantEffByType", "MENA.EDS"])
  MENA_iCO2CaptRate <- readSource("MENA_EDS", subtype =  map[map[["OPEN.PROM"]] == "iCO2CaptRate", "MENA.EDS"])
  
  MENA_sum1 <- MENA_iCo2EmiFac[,,INDDOM[, 1]] * MENA_VConsFuel[,,INDDOM[, 1]]
  MENA_sum1 <- dimSums(MENA_sum1, 3, na.rm = TRUE)
  
  MENA_sum2 <- MENA_VTransfInThermPowPls[,,PGEF[,1]]*MENA_iCo2EmiFac[,,"PG"][,,PGEF[,1]]
  MENA_sum2 <- dimSums(MENA_sum2, 3, na.rm = TRUE)
  
  MENA_sum3 <- MENA_VTransfInputDHPlants[,,] * MENA_iCo2EmiFac[,,"PG"][,,getItems(MENA_VTransfInputDHPlants,3)]
  MENA_sum3 <- dimSums(MENA_sum3, 3, na.rm = TRUE)
  
  MENA_sum4 <- MENA_VEnCons * MENA_iCo2EmiFac[,,"PG"][,,getItems(MENA_VEnCons,3)]
  MENA_sum4 <- dimSums(MENA_sum4, 3, na.rm = TRUE)
  
  MENA_sum5 <- MENA_VDemTr[,,map_TRANSECTOR[, 1]] * MENA_iCo2EmiFac[,,map_TRANSECTOR[, 1]]
  MENA_sum5 <- dimSums(MENA_sum5, 3, na.rm = TRUE)
  
  MENA_var_16 <- MENA_VElecProd[,,CCS[,1]] * 0.086 / MENA_iPlantEffByType[,,CCS[,1]] * MENA_iCo2EmiFac[,,"PG"][,,CCS[,2]] * MENA_iCO2CaptRate[,,CCS[,1]]
  MENA_sum6 <- dimSums(MENA_var_16,dim=3, na.rm = TRUE)
  
  MENA_sum7 <- MENA_iCo2EmiFac[,,SECTTECH2[,1]] * MENA_VConsFuel[,,SECTTECH2[,1]]
  MENA_sum7 <- dimSums(MENA_sum7,dim=3, na.rm = TRUE)
  
  MENA_SUM <- MENA_sum1 + MENA_sum2 + MENA_sum3 + MENA_sum4 + MENA_sum5 - MENA_sum6 + MENA_sum7
  
  getItems(MENA_SUM, 3) <- paste0("Emissions|CO2")
  
  getRegions(MENA_SUM) <- sub("MOR", "MAR", getRegions(MENA_SUM))
  
  # choose years and regions that both models have
  years <- c(fStartHorizon : max(getYears(MENA_SUM, as.integer = TRUE)))
  getItems(MENA_SUM, 3.1) <- paste0("Emissions|CO2")
  
  MENA_SUM <- as.quitte(MENA_SUM) %>%
    interpolate_missing_periods(period = getYears(MENA_SUM,as.integer=TRUE)[1]:getYears(MENA_SUM,as.integer=TRUE)[length(getYears(MENA_SUM))], expand.values = TRUE)
  
  MENA_SUM <- as.quitte(MENA_SUM) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(MENA_SUM, as.integer = TRUE)]
  
  MENA_SUM_GLO <- dimSums(MENA_SUM, 1)
  getItems(MENA_SUM_GLO, 1) <- "World"
  MENA_SUM <- mbind(MENA_SUM, MENA_SUM_GLO)
  
  # write data in mif file
  write.report(MENA_SUM[, years_in_horizon, ], file = "reporting.mif", model = "MENA-EDS", unit = "Mt CO2/yr", append = TRUE, scenario = "Baseline")
  
  # Emissions|CO2|Cumulated
  
  Cumulated_MENA <- as.quitte(MENA_SUM[, years_in_horizon, ])
  
  Cumulated_MENA <- Cumulated_MENA %>% group_by(region) %>% mutate(value = cumsum(value))
  
  Cumulated_MENA <- as.data.frame(Cumulated_MENA)
  
  Cumulated_MENA <- as.quitte(Cumulated_MENA) %>% as.magpie()
  
  getItems(Cumulated_MENA, 3.1) <- paste0("Emissions|CO2|Cumulated")
  
  Cumulated_MENA <- Cumulated_MENA / 1000
  
  # write data in mif file
  write.report(Cumulated_MENA, file = "reporting.mif",model = "MENA-EDS",unit = "Gt CO2",append = TRUE,scenario = "Baseline")
  
  # filter ENERDATA by number 2
  number_2 <- readSource("ENERDATA", "2", convert = TRUE)
  CO2_emissions_ENERDATA <- number_2[, , "CO2 emissions from fuel combustion (sectoral approach).MtCO2"]
  
  year <- Reduce(intersect, list(years, getYears(CO2_emissions_ENERDATA, as.integer=TRUE)))
  
  getItems(CO2_emissions_ENERDATA, 3) <- paste0("Emissions|CO2")
  
  # aggregation
  CO2_emissions_ENERDATA[is.na(CO2_emissions_ENERDATA)] <- 0
  CO2_emissions_ENERDATA <- toolAggregate(CO2_emissions_ENERDATA, rel = rmap)
  
  CO2_emissions_ENERDATA <- as.quitte(CO2_emissions_ENERDATA) %>%
    interpolate_missing_periods(period = getYears(CO2_emissions_ENERDATA,as.integer=TRUE)[1]:getYears(CO2_emissions_ENERDATA,as.integer=TRUE)[length(getYears(CO2_emissions_ENERDATA))], expand.values = TRUE)
  
  CO2_emissions_ENERDATA <- as.quitte(CO2_emissions_ENERDATA) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(CO2_emissions_ENERDATA, as.integer = TRUE)]
  
  CO2_emissions_ENERDATA_GLO <- dimSums(CO2_emissions_ENERDATA, 1)
  getItems(CO2_emissions_ENERDATA_GLO, 1) <- "World"
  CO2_emissions_ENERDATA <- mbind(CO2_emissions_ENERDATA, CO2_emissions_ENERDATA_GLO)
  
  # write data in mif file
  write.report(CO2_emissions_ENERDATA[, years_in_horizon, ], file = "reporting.mif", model = "ENERDATA", unit = "Mt CO2/yr", append = TRUE, scenario = "Validation")
  
  # Emissions|CO2|Cumulated
  
  Cumulated_ENERDATA <- as.quitte(CO2_emissions_ENERDATA[, years_in_horizon, ])
  
  Cumulated_ENERDATA <- Cumulated_ENERDATA %>% group_by(region) %>% mutate(value = cumsum(value))
  
  Cumulated_ENERDATA <- as.data.frame(Cumulated_ENERDATA)
  
  Cumulated_ENERDATA <- as.quitte(Cumulated_ENERDATA) %>% as.magpie()
  
  getItems(Cumulated_ENERDATA, 3) <- paste0("Emissions|CO2|Cumulated")
  
  Cumulated_ENERDATA <- Cumulated_ENERDATA /1000
  
  # write data in mif file
  write.report(Cumulated_ENERDATA, file = "reporting.mif",model = "ENERDATA",unit = "Gt CO2", append = TRUE, scenario = "Validation")
  
  # EDGAR emissions
  EDGAR <- calcOutput(type = "CO2_emissions", aggregate = TRUE)
  getItems(EDGAR, 3) <- paste0("Emissions|CO2")
  
  EDGAR <- as.quitte(EDGAR) %>%
    interpolate_missing_periods(period = getYears(EDGAR,as.integer=TRUE)[1]:getYears(EDGAR,as.integer=TRUE)[length(getYears(EDGAR))], expand.values = TRUE)
  
  EDGAR <- as.quitte(EDGAR) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(EDGAR, as.integer = TRUE)]
  
  EDGAR_GLO <- dimSums(EDGAR, 1)
  getItems(EDGAR_GLO, 1) <- "World"
  EDGAR <- mbind(EDGAR, EDGAR_GLO)
  
  write.report(EDGAR[, years_in_horizon, ], file = "reporting.mif", model = "EDGAR", unit = "Mt CO2/yr", append=TRUE, scenario = "Validation")
  
  # Emissions|CO2|Cumulated
  
  Cumulated_EDGAR <- as.quitte(EDGAR[, years_in_horizon, ])
  
  Cumulated_EDGAR <- Cumulated_EDGAR %>% group_by(region) %>% mutate(value = cumsum(value))
  
  Cumulated_EDGAR <- as.data.frame(Cumulated_EDGAR)
  
  Cumulated_EDGAR <- as.quitte(Cumulated_EDGAR) %>% as.magpie()
  
  getItems(Cumulated_EDGAR, 3) <- paste0("Emissions|CO2|Cumulated")
  
  Cumulated_EDGAR <- Cumulated_EDGAR / 1000
  
  # write data in mif file
  write.report(Cumulated_EDGAR, file = "reporting.mif", model = "EDGAR",unit = "Gt CO2", append = TRUE, scenario = "Validation")
  
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
  write.report(pik[, years_in_horizon, ], file = "reporting.mif", model = "PIK", unit = "Mt CO2/yr", append = TRUE, scenario = "Validation")
  
  # Emissions|CO2|Cumulated
  
  Cumulated_pik <- as.quitte(pik[, years_in_horizon, ])
  
  Cumulated_pik <- Cumulated_pik %>% group_by(region) %>% mutate(value = cumsum(value))
  
  Cumulated_pik <- as.data.frame(Cumulated_pik)
  
  Cumulated_pik <- as.quitte(Cumulated_pik) %>% as.magpie()
  
  getItems(Cumulated_pik, 3) <- paste0("Emissions|CO2|Cumulated")
  
  Cumulated_pik <- Cumulated_pik / 1000
  
  # write data in mif file
  write.report(Cumulated_pik, file = "reporting.mif", model = "PIK", unit = "Gt CO2", append = TRUE, scenario = "Validation")
  
  # Navigate CO2 emissions
  x1 <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = TRUE)
  x1 <- x1[,,"Emissions|CO2"][,,"Mt CO2/yr"]
  world_Navigate_NPi_CO2 <- world_Navigate_NPi[,,"Emissions|CO2"][,,"Mt CO2/yr"]
  years <- intersect(getYears(x1,as.integer=TRUE), getYears(world_Navigate_NPi_CO2, as.integer = TRUE))
  x1 <- mbind(x1[,years,], world_Navigate_NPi_CO2[,years,])
  
  x2 <- readSource("Navigate", subtype = "SUP_1p5C_Default", convert = TRUE)
  x2 <- x2[,,"Emissions|CO2"][,,"Mt CO2/yr"]
  world_Navigate_1p5C_CO2 <- world_Navigate_1p5C[,,"Emissions|CO2"][,,"Mt CO2/yr"]
  x2 <- mbind(x2[,years,], world_Navigate_1p5C_CO2[,years,])
  
  x3 <- readSource("Navigate", subtype = "SUP_2C_Default", convert = TRUE)
  x3 <- x3[,,"Emissions|CO2"][,,"Mt CO2/yr"]
  world_Navigate_2C_CO2 <- world_Navigate_2C[,,"Emissions|CO2"][,,"Mt CO2/yr"]
  x3 <- mbind(x3[,years,], world_Navigate_2C_CO2[,years,])
  
  # keep common years that exist in the scenarios
  x1 <- x1[, Reduce(intersect, list(getYears(x1), getYears(x2), getYears(x3))), ]
  x2 <- x2[, Reduce(intersect, list(getYears(x1), getYears(x2), getYears(x3))), ]
  x3 <- x3[, Reduce(intersect, list(getYears(x1), getYears(x2), getYears(x3))), ]

  Navigate_data <- mbind(x1, x2, x3)
  
  Navigate_CO2 <- Navigate_data[,,"Emissions|CO2"][,,"Mt CO2/yr"]
  
  year <- Reduce(intersect, list(c(fStartHorizon : 2100)), getYears(Navigate_CO2, as.integer = TRUE))
  
  # aggregation
  Navigate_CO2[is.na(Navigate_CO2)] <- 0
  
  Navigate_CO2_world <- Navigate_CO2["World",,]
  Navigate_CO2 <- Navigate_CO2[as.character(getISOlist()), , ]
  Navigate_CO2 <- toolAggregate(Navigate_CO2, rel = rmap)
  Navigate_CO2 <- mbind(Navigate_CO2, Navigate_CO2_world)
  
  Navigate_CO2 <- as.quitte(Navigate_CO2) %>%
    interpolate_missing_periods(period = getYears(Navigate_CO2,as.integer=TRUE)[1]:getYears(Navigate_CO2,as.integer=TRUE)[length(getYears(Navigate_CO2))], expand.values = TRUE)
  
  Navigate_CO2 <- as.quitte(Navigate_CO2) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(Navigate_CO2, as.integer = TRUE)]
  
  # write data in mif file
  write.report(Navigate_CO2[, years_in_horizon, ], file = "reporting.mif", model = "Navigate", unit = "Mt CO2", append = TRUE)
  
  # Emissions|CO2|Cumulated
  
  Cumulated_Navigate <- as.quitte(Navigate_CO2[, years_in_horizon, ])
  
  model <- NULL
  scenario <- NULL
  
  Cumulated_Navigate <- Cumulated_Navigate %>% group_by(region, model, scenario) %>%
    mutate(value = cumsum(value))
  
  Cumulated_Navigate <- as.data.frame(Cumulated_Navigate)
  
  Cumulated_Navigate <- as.quitte(Cumulated_Navigate) %>% as.magpie()
  
  getItems(Cumulated_Navigate, 3.3) <- paste0("Emissions|CO2|Cumulated")
  getItems(Cumulated_Navigate, 3.4) <- paste0("Gt CO2")
  
  Cumulated_Navigate <- Cumulated_Navigate / 1000
  
  # write data in mif file
  write.report(Cumulated_Navigate, file = "reporting.mif", model = "Navigate", unit = "Gt CO2", append = TRUE)
  
  # Navigate CH4 emissions
  x1 <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = TRUE)
  x1 <- x1[,,"Emissions|CH4"][,,"Mt CH4/yr"]
  world_Navigate_NPi_CH4 <- world_Navigate_NPi[,,"Emissions|CH4"][,,"Mt CH4/yr"]
  years <- intersect(getYears(x1,as.integer=TRUE), getYears(world_Navigate_NPi_CH4, as.integer = TRUE))
  x1 <- mbind(x1[,years,], world_Navigate_NPi_CH4[,years,])
  
  x2 <- readSource("Navigate", subtype = "SUP_1p5C_Default", convert = TRUE)
  x2 <- x2[,,"Emissions|CH4"][,,"Mt CH4/yr"]
  world_Navigate_1p5C_CH4 <- world_Navigate_1p5C[,,"Emissions|CH4"][,,"Mt CH4/yr"]
  x2 <- mbind(x2[,years,], world_Navigate_1p5C_CH4[,years,])
  
  x3 <- readSource("Navigate", subtype = "SUP_2C_Default", convert = TRUE)
  x3 <- x3[,,"Emissions|CH4"][,,"Mt CH4/yr"]
  world_Navigate_2C_CH4 <- world_Navigate_2C[,,"Emissions|CH4"][,,"Mt CH4/yr"]
  x3 <- mbind(x3[,years,], world_Navigate_2C_CH4[,years,])
  
  # keep common years that exist in the scenarios
  x1 <- x1[, Reduce(intersect, list(getYears(x1), getYears(x2), getYears(x3))), ]
  x2 <- x2[, Reduce(intersect, list(getYears(x1), getYears(x2), getYears(x3))), ]
  x3 <- x3[, Reduce(intersect, list(getYears(x1), getYears(x2), getYears(x3))), ]
  
  Navigate_data <- mbind(x1, x2, x3)
  
  Navigate_CH4 <- Navigate_data[,,"Emissions|CH4"][,,"Mt CH4/yr"]

  year <- Reduce(intersect, list(c(fStartHorizon : 2100)), getYears(Navigate_CH4, as.integer = TRUE))
  
  # aggregation
  Navigate_CH4[is.na(Navigate_CH4)] <- 0
  
  Navigate_CH4_world <- Navigate_CH4["World",,]
  Navigate_CH4 <- Navigate_CH4[as.character(getISOlist()), , ]
  Navigate_CH4 <- toolAggregate(Navigate_CH4, rel = rmap)
  Navigate_CH4 <- mbind(Navigate_CH4, Navigate_CH4_world)
  
  Navigate_CH4 <- as.quitte(Navigate_CH4) %>%
    interpolate_missing_periods(period = getYears(Navigate_CH4,as.integer=TRUE)[1]:getYears(Navigate_CH4,as.integer=TRUE)[length(getYears(Navigate_CH4))], expand.values = TRUE)
  
  Navigate_CH4 <- as.quitte(Navigate_CH4) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(Navigate_CH4, as.integer = TRUE)]
  
  # write data in mif file
  write.report(Navigate_CH4[, years_in_horizon, ], file = "reporting.mif", model = "Navigate", unit = "Mt CH4", append = TRUE)
  
  # Navigate NOx emissions
  x1 <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = TRUE)
  x1 <- x1[,,"Emissions|NOx"][,,"Mt NO2/yr"]
  world_Navigate_NPi_NO2 <- world_Navigate_NPi[,,"Emissions|NOx"][,,"Mt NO2/yr"]
  years <- intersect(getYears(x1,as.integer=TRUE), getYears(world_Navigate_NPi_NO2, as.integer = TRUE))
  x1 <- mbind(x1[,years,], world_Navigate_NPi_NO2[,years,])
  
  x2 <- readSource("Navigate", subtype = "SUP_1p5C_Default", convert = TRUE)
  x2 <- x2[,,"Emissions|NOx"][,,"Mt NO2/yr"]
  world_Navigate_1p5C_NO2 <- world_Navigate_1p5C[,,"Emissions|NOx"][,,"Mt NO2/yr"]
  x2 <- mbind(x2[,years,], world_Navigate_1p5C_NO2[,years,])
  
  x3 <- readSource("Navigate", subtype = "SUP_2C_Default", convert = TRUE)
  x3 <- x3[,,"Emissions|NOx"][,,"Mt NO2/yr"]
  world_Navigate_2C_NO2 <- world_Navigate_2C[,,"Emissions|NOx"][,,"Mt NO2/yr"]
  x3 <- mbind(x3[,years,], world_Navigate_2C_NO2[,years,])
  
  # keep common years that exist in the scenarios
  x1 <- x1[, Reduce(intersect, list(getYears(x1), getYears(x2), getYears(x3))), ]
  x2 <- x2[, Reduce(intersect, list(getYears(x1), getYears(x2), getYears(x3))), ]
  x3 <- x3[, Reduce(intersect, list(getYears(x1), getYears(x2), getYears(x3))), ]
  
  Navigate_data <- mbind(x1, x2, x3)
  
  Navigate_NOx <- Navigate_data[,,"Emissions|NOx"][,,"Mt NO2/yr"]
  
  year <- Reduce(intersect, list(c(fStartHorizon : 2100)),getYears(Navigate_NOx, as.integer = TRUE))
  
  # aggregation
  Navigate_NOx[is.na(Navigate_NOx)] <- 0
  
  Navigate_NOx_world <- Navigate_NOx["World",,]
  Navigate_NOx <- Navigate_NOx[as.character(getISOlist()), , ]
  Navigate_NOx <- toolAggregate(Navigate_NOx, rel = rmap)
  Navigate_NOx <- mbind(Navigate_NOx, Navigate_NOx_world)
  
  Navigate_NOx <- as.quitte(Navigate_NOx) %>%
    interpolate_missing_periods(period = getYears(Navigate_NOx,as.integer=TRUE)[1]:getYears(Navigate_NOx,as.integer=TRUE)[length(getYears(Navigate_NOx))], expand.values = TRUE)
  
  Navigate_NOx <- as.quitte(Navigate_NOx) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(Navigate_NOx, as.integer = TRUE)]
  
  # write data in mif file
  write.report(Navigate_NOx[, years_in_horizon, ], file = "reporting.mif", model = "Navigate", unit = "Mt NO2", append = TRUE)
  
  ########### electricity production by source
  # load data source (ENERDATA)
  x <- readSource("ENERDATA", "production", convert = TRUE)
  prod <- x

  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]

  x <- x[, c(max(fStartHorizon, min(getYears(x, as.integer = TRUE))) : max(getYears(x, as.integer = TRUE))), ]

  # load current OPENPROM set configuration
  sets <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "PGALL")
  sets <- unlist(strsplit(sets[, 1], ","))

  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-enerdata-elecprod-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")

  ## filter mapping to keep only XXX sectors
  map <- filter(map, map[, "PGALL"] %in% sets)
  ## ..and only items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  map <- map[map[, "ENERDATA"] %in% enernames, ]
  ## filter data to keep only XXX data
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  x <- x[, , enernames]
  ## rename variables to openprom names
  getItems(x, 3.1) <- map[map[["ENERDATA"]] %in% paste0(getItems(x, 3.1), ".GWh"), "PGALL"]

  # set NA to 0
  x[is.na(x)] <- 0

  elc_prod <- x

  # map of enerdata, OPEN-PROM, elec prod
  map_reporting <- toolGetMapping(name = "enerdata-elec-prod.csv",
                                 type = "sectoral",
                                 where = "mrprom")

  # aggregate from ENERDATA fuels to reporting fuel categories
  elc_prod <- toolAggregate(elc_prod,dim = 3.1,rel = map_reporting,from = "OPEN.PROM",to = "REPORTING")

  getItems(elc_prod, 3.1) <- paste0("Secondary Energy|Electricity|", getItems(elc_prod, 3.1))
  getItems(elc_prod, 3) <- getItems(elc_prod, 3.1)

  elc_prod <- toolAggregate(elc_prod, rel = rmap)

  elc_prod <- as.quitte(elc_prod) %>%
    interpolate_missing_periods(period = getYears(elc_prod,as.integer=TRUE)[1]:getYears(elc_prod,as.integer=TRUE)[length(getYears(elc_prod))], expand.values = TRUE)
  
  elc_prod <- as.quitte(elc_prod) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(elc_prod, as.integer = TRUE)]
  
  elc_prod <- elc_prod /1000 # GWh to TWh
  
  elc_prod_GLO <- dimSums(elc_prod, 1)
  getItems(elc_prod_GLO, 1) <- "World"
  elc_prod <- mbind(elc_prod, elc_prod_GLO)
  
  # write data in mif file
  write.report(elc_prod[, years_in_horizon, ], file = "reporting.mif", model = "ENERDATA", unit = "TWh", append = TRUE, scenario = "Validation")

  # Electricity Total
  elc_total <- dimSums(elc_prod, dim = 3, na.rm = TRUE)

  getItems(elc_total, 3) <- paste0("Secondary Energy|Electricity")

  elc_total <- as.quitte(elc_total) %>%
    interpolate_missing_periods(period = getYears(elc_total,as.integer=TRUE)[1]:getYears(elc_total,as.integer=TRUE)[length(getYears(elc_total))], expand.values = TRUE)
  
  elc_total <- as.quitte(elc_total) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(elc_total, as.integer = TRUE)]
  
  # write data in mif file
  write.report(elc_total[, years_in_horizon, ], file = "reporting.mif", model = "ENERDATA", unit = "TWh", append = TRUE, scenario = "Validation")

  # Navigate SE

  # map of Navigate, OPEN-PROM, elec prod
  map_reporting_Navigate <- toolGetMapping(name = "navigate-elec-prod.csv",
                                  type = "sectoral",
                                  where = "mrprom")
  
  x1 <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = TRUE)
  x1 <- x1[,,map_reporting_Navigate[,"Navigate"]]
  world_Navigate_NPi_total <- world_Navigate_NPi[,,map_reporting_Navigate[,"Navigate"]]
  years <- intersect(getYears(x1,as.integer=TRUE), getYears(world_Navigate_NPi_total, as.integer = TRUE))
  x1 <- mbind(x1[,years,], world_Navigate_NPi_total[,years,])
  
  x2 <- readSource("Navigate", subtype = "SUP_1p5C_Default", convert = TRUE)
  x2 <- x2[,,map_reporting_Navigate[,"Navigate"]]
  world_Navigate_1p5C_total <- world_Navigate_1p5C[,,map_reporting_Navigate[,"Navigate"]]
  x2 <- mbind(x2[,years,], world_Navigate_1p5C_total[,years,])
  
  x3 <- readSource("Navigate", subtype = "SUP_2C_Default", convert = TRUE)
  x3 <- x3[,,map_reporting_Navigate[,"Navigate"]]
  world_Navigate_2C_total <- world_Navigate_2C[,,map_reporting_Navigate[,"Navigate"]]
  x3 <- mbind(x3[,years,], world_Navigate_2C_total[,years,])
  
  # keep common years that exist in the scenarios
  x1 <- x1[, Reduce(intersect, list(getYears(x1), getYears(x2), getYears(x3))), ]
  x2 <- x2[, Reduce(intersect, list(getYears(x1), getYears(x2), getYears(x3))), ]
  x3 <- x3[, Reduce(intersect, list(getYears(x1), getYears(x2), getYears(x3))), ]
  
  Navigate_data <- mbind(x1, x2, x3)
  
  Navigate_data <- Navigate_data[,,map_reporting_Navigate[,"Navigate"]]

  # filter data to keep only Navigate map variables
  navigate_SE <- Navigate_data[,,map_reporting_Navigate[,"Navigate"]] * 277.778 # EJ to TWh

  # choose years
  navigate_SE <- navigate_SE[, getYears(navigate_SE, as.integer = T) %in% c(fStartHorizon : 2100), ]
  year <- getYears(navigate_SE)

  # EJ to Mtoe
  getItems(navigate_SE, 3.4) <- "TWh"

  # aggregate from Navigate SE to reporting categories
  navigate_SE <- toolAggregate(navigate_SE[, year, ], dim = 3.3,rel = map_reporting_Navigate, from = "Navigate", to = "SE")

  # country aggregation
  
  navigate_SE_world <- navigate_SE["World",,]
  navigate_SE <- navigate_SE[as.character(getISOlist()), , ]
  navigate_SE <- toolAggregate(navigate_SE, rel = rmap)
  navigate_SE <- mbind(navigate_SE, navigate_SE_world)

  navigate_SE[is.na(navigate_SE)] <- 0

  navigate_SE <- as.quitte(navigate_SE) %>%
    interpolate_missing_periods(period = getYears(navigate_SE,as.integer=TRUE)[1]:getYears(navigate_SE,as.integer=TRUE)[length(getYears(navigate_SE))], expand.values = TRUE)
  
  navigate_SE <- as.quitte(navigate_SE) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(navigate_SE, as.integer = TRUE)]
  
  # write data in mif file
  write.report(navigate_SE[, years_in_horizon, ], file = "reporting.mif", append = TRUE)
  
  
  ########### primary energy by source
  
  # load data source (ENERDATA)
  x <- readSource("ENERDATA", "production", convert = TRUE)
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  x <- x[, c(max(fStartHorizon, min(getYears(x, as.integer = TRUE))) : max(getYears(x, as.integer = TRUE))), ]
  
  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-reporting-primaryproduction-mapping.csv",
                                                    type = "sectoral",
                                                    where = "mrprom")
  
  ## ..and only items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  map <- map[map[, "ENERDATA"] %in% enernames, ]
  ## filter data
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  x <- x[, , enernames]
  
  # set NA to 0
  x[is.na(x)] <- 0
  
  # aggregate from ENERDATA fuels to reporting fuel categories
  prim_prod <- toolAggregate(x,dim = 3.1,rel = map,from = "ENERDATA",to = "Reporting")
  
  prim_prod <- toolAggregate(prim_prod, rel = rmap)
  
  prim_prod <- as.quitte(prim_prod) %>%
    interpolate_missing_periods(period = getYears(prim_prod,as.integer=TRUE)[1]:getYears(prim_prod,as.integer=TRUE)[length(getYears(prim_prod))], expand.values = TRUE)
  
  prim_prod <- as.quitte(prim_prod) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(prim_prod, as.integer = TRUE)]
  
  prim_prod_GLO <- dimSums(prim_prod, 1)
  getItems(prim_prod_GLO, 1) <- "World"
  prim_prod <- mbind(prim_prod, prim_prod_GLO)
  
  # write data in mif file
  write.report(prim_prod[, years_in_horizon, ], file = "reporting.mif", model = "ENERDATA", unit = "Mtoe", append = TRUE, scenario = "Validation")
  
  # primary production Total
  prim_total <- dimSums(prim_prod, dim = 3, na.rm = TRUE)
  
  getItems(prim_total, 3) <- paste0("Primary Energy")
  
  prim_total <- as.quitte(prim_total) %>%
    interpolate_missing_periods(period = getYears(prim_total,as.integer=TRUE)[1]:getYears(prim_total,as.integer=TRUE)[length(getYears(prim_total))], expand.values = TRUE)
  
  prim_total <- as.quitte(prim_total) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(prim_total, as.integer = TRUE)]
  
  # write data in mif file
  write.report(prim_total[, years_in_horizon, ], file = "reporting.mif", model = "ENERDATA", unit = "Mtoe", append = TRUE, scenario = "Validation")
  
  # Navigate PE
  
  x1 <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = TRUE)
  
  z <- as.data.frame(getItems(x1,3.3))
  
  get_items <- z[grep("^Primary Energy", getItems(x1,3.3)),1]
  
  x1 <- x1[,,get_items]
  
  Navigate_data <- x1
  
  # filter data to keep only Navigate map variables
  navigate_PE <- Navigate_data[,,get_items] * 23.8846 # EJ to Mtoe
  
  # EJ to Mtoe
  getItems(navigate_PE, 3.4) <- "Mtoe"
  
  # country aggregation
  navigate_PE <- toolAggregate(navigate_PE, rel = rmap)
  
  navigate_PE[is.na(navigate_PE)] <- 0
  
  navigate_PE <- as.quitte(navigate_PE) %>%
    interpolate_missing_periods(period = getYears(navigate_PE,as.integer=TRUE)[1]:getYears(navigate_PE,as.integer=TRUE)[length(getYears(navigate_PE))], expand.values = TRUE)
  
  navigate_PE <- as.quitte(navigate_PE) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(navigate_PE, as.integer = TRUE)]
  
  navigate_PE_GLO <- dimSums(navigate_PE, 1)
  getItems(navigate_PE_GLO, 1) <- "World"
  navigate_PE <- mbind(navigate_PE, navigate_PE_GLO)
  
  # write data in mif file
  write.report(navigate_PE[, years_in_horizon, ], file = "reporting.mif", append = TRUE)
  
  # IEA PE
  
  map <- toolGetMapping(name = "prom-reporting-primaryproduction-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")
  
  IEA_PE <- readSource("IEA", subtype = "INDPROD") / 1000 #ktoe to Mtoe
  
  IEA_PE <- as.quitte(IEA_PE) 
  
  IEA_COAL <- IEA_PE

  IEA_PE <- filter(IEA_PE, IEA_PE[["product"]] %in% map[, "IEA"])
  
  # aggregate lingite, cokcoal, antcoal,  bitcoal to COAL
  IEA_COAL <- filter(IEA_COAL, IEA_COAL[["product"]] %in% c("BITCOAL", "COKCOAL", "ANTCOAL", "LIGNITE"))
  IEA_COAL <- select((IEA_COAL), c(region, period, value))
  IEA_COAL <- mutate(IEA_COAL, value = sum(value, na.rm = TRUE), .by = c("period", "region"))
  IEA_COAL <- distinct(IEA_COAL)
  IEA_PE <- left_join(IEA_PE, IEA_COAL, by = c("region", "period"))
  
  IEA_PE[which(IEA_PE[, 8] == "BITCOAL"),] <- IEA_PE[which(IEA_PE[, 8] == "BITCOAL"),] %>% mutate(`value.x` = ifelse(is.na(`value.y`), `value.x`, `value.y`))
  names(IEA_PE) <- sub("value.x", "value", names(IEA_PE))
  IEA_PE <- select((IEA_PE), -c(`value.y`))
  
  IEA_PE <- as.quitte(IEA_PE) %>% as.magpie()
  
  getItems(IEA_PE, 3) <- getItems(IEA_PE, 3.2)
  
  # aggregate from IEA fuels to reporting fuel categories
  IEA_PE <- toolAggregate(IEA_PE,dim = 3,rel = map,from = "IEA",to = "Reporting")
  
  IEA_PE <- toolAggregate(IEA_PE, rel = rmap)
  
  IEA_PE <- as.quitte(IEA_PE) %>%
    interpolate_missing_periods(period = getYears(IEA_PE,as.integer=TRUE)[1]:getYears(IEA_PE,as.integer=TRUE)[length(getYears(IEA_PE))], expand.values = TRUE)
  
  IEA_PE <- as.quitte(IEA_PE) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(IEA_PE, as.integer = TRUE)]
  
  IEA_PE_GLO <- dimSums(IEA_PE, 1)
  getItems(IEA_PE_GLO, 1) <- "World"
  IEA_PE <- mbind(IEA_PE, IEA_PE_GLO)
  
  # write data in mif file
  write.report(IEA_PE[, years_in_horizon, ], file = "reporting.mif", model = "IEA_WB", unit = "Mtoe", append = TRUE, scenario = "Validation")
  
  # add extra emissions
  SUP_NPi_Default <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = TRUE)
  SUP_NPi_Default_W <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = FALSE)
  world_Navigate_NPi <- SUP_NPi_Default_W["World",,]
  
  # map with the extra emissions
  map_extra_emissions <- toolGetMapping(name = "navigate-extra-emissions.csv",
                                        type = "sectoral",
                                        where = "mrprom")
  
  # Navigate CO2 emissions mapping
  Navigate_CO2 <- SUP_NPi_Default[,,map_extra_emissions[,"Navigate"]]
  
  # aggregation
  Navigate_CO2[is.na(Navigate_CO2)] <- 0
  world_Navigate_NPi[is.na(world_Navigate_NPi)] <- 0
  
  Navigate_CO2_world <- world_Navigate_NPi[,,map_extra_emissions[,"Navigate"]]
  Navigate_CO2 <- Navigate_CO2[as.character(getISOlist()), , ]
  Navigate_CO2 <- toolAggregate(Navigate_CO2, rel = rmap)
  
  # keep common years that exist in the scenarios
  Navigate_CO2 <- Navigate_CO2[, Reduce(intersect, list(getYears(Navigate_CO2), getYears(Navigate_CO2_world))), ]
  Navigate_CO2_world <- Navigate_CO2_world[, Reduce(intersect, list(getYears(Navigate_CO2), getYears(Navigate_CO2_world))), ]
  
  Navigate_CO2 <- mbind(Navigate_CO2, Navigate_CO2_world)
  
  Navigate_CO2 <- as.quitte(Navigate_CO2) %>%
    interpolate_missing_periods(period = getYears(Navigate_CO2,as.integer=TRUE)[1]:getYears(Navigate_CO2,as.integer=TRUE)[length(getYears(Navigate_CO2))], expand.values = TRUE)
  
  Navigate_CO2 <- as.quitte(Navigate_CO2) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(Navigate_CO2, as.integer = TRUE)]
  
  # write data in mif file
  write.report(Navigate_CO2[, years_in_horizon, ], file = "reporting.mif", model = "Navigate", append = TRUE)
  
  # add GDP and POP
  Navigate_GDP <- SUP_NPi_Default[,, c("GDP|MER", "GDP|PPP")] * 1.09 #US$2010 to US$2015
  getItems(Navigate_GDP, 3.4) <- "billion US$2015/yr"
  Navigate_GDP_w <- world_Navigate_NPi[,, c("GDP|MER", "GDP|PPP")] * 1.09 #US$2010 to US$2015
  getItems(Navigate_GDP_w, 3.4) <- "billion US$2015/yr"
  
  # aggregation
  Navigate_GDP[is.na(Navigate_GDP)] <- 0
  Navigate_GDP_w[is.na(Navigate_GDP_w)] <- 0
  
  Navigate_GDP <- Navigate_GDP[as.character(getISOlist()), , ]
  Navigate_GDP <- toolAggregate(Navigate_GDP, rel = rmap)
  
  # keep common years that exist in the scenarios
  Navigate_GDP <- Navigate_GDP[, Reduce(intersect, list(getYears(Navigate_GDP), getYears(Navigate_GDP_w))), ]
  Navigate_GDP_w <- Navigate_GDP_w[, Reduce(intersect, list(getYears(Navigate_GDP), getYears(Navigate_GDP_w))), ]
  
  Navigate_GDP <- mbind(Navigate_GDP, Navigate_GDP_w)
  
  Navigate_GDP <- as.quitte(Navigate_GDP) %>%
    interpolate_missing_periods(period = getYears(Navigate_GDP,as.integer=TRUE)[1]:getYears(Navigate_GDP,as.integer=TRUE)[length(getYears(Navigate_GDP))], expand.values = TRUE)
  
  Navigate_GDP <- as.quitte(Navigate_GDP) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(Navigate_GDP, as.integer = TRUE)]
  
  # write data in mif file
  write.report(Navigate_GDP[, years_in_horizon, ], file = "reporting.mif", model = "Navigate", append = TRUE)
  
  Navigate_POP <- SUP_NPi_Default[,, "Population"] / 1000 #million to billion
  getItems(Navigate_POP, 3.4) <- "billion"
  Navigate_POP_w <- world_Navigate_NPi[,, "Population"] / 1000 #million to billion
  getItems(Navigate_POP_w, 3.4) <- "billion"
  
  # aggregation
  Navigate_POP[is.na(Navigate_POP)] <- 0
  Navigate_POP_w[is.na(Navigate_POP_w)] <- 0
  
  Navigate_POP <- Navigate_POP[as.character(getISOlist()), , ]
  Navigate_POP <- toolAggregate(Navigate_POP, rel = rmap)
  
  # keep common years that exist in the scenarios
  Navigate_POP <- Navigate_POP[, Reduce(intersect, list(getYears(Navigate_POP), getYears(Navigate_POP_w))), ]
  Navigate_POP_w <- Navigate_POP_w[, Reduce(intersect, list(getYears(Navigate_POP), getYears(Navigate_POP_w))), ]
  
  Navigate_POP <- mbind(Navigate_POP, Navigate_POP_w)
  
  Navigate_POP <- as.quitte(Navigate_POP) %>%
    interpolate_missing_periods(period = getYears(Navigate_POP,as.integer=TRUE)[1]:getYears(Navigate_POP,as.integer=TRUE)[length(getYears(Navigate_POP))], expand.values = TRUE)
  
  Navigate_POP <- as.quitte(Navigate_POP) %>% as.magpie()
  years_in_horizon <-  horizon[horizon %in% getYears(Navigate_POP, as.integer = TRUE)]
  
  # write data in mif file
  write.report(Navigate_POP[, years_in_horizon, ], file = "reporting.mif", model = "Navigate", append = TRUE)
  
  # rename mif file
  fullVALIDATION <- read.report("reporting.mif")
  write.report(fullVALIDATION, file = paste0("fullVALIDATION.mif"))
  
  return(list(x = x,
              weight = NULL,
              unit = "Mtoe",
              description = "VALIDATION"))
  
}
