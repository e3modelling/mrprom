#' calcIInstCapPast2
#'
#' Use data from IRENA to derive OPENPROM input parameter iInstCapPast2
#' This dataset contains the values of installed capacity for past years in GW.
#'
#' @return  OPENPROM input data iInstCapPast
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios, Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IInstCapPast2", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr full_join group_by mutate if_else ungroup select rename filter
#' @importFrom tidyr separate_rows
#' @importFrom quitte as.quitte
#' @importFrom tibble add_row

calcIInstCapPast2 <- function(mode = "TotalEff") {
  
  fStartHorizon <- toolReadEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fStartHorizon"]
  
  PGALL <- toolGetMapping(
    name = "PGALL.csv",
    type = "blabla_export",
    where = "mrprom"
  )
  
  IRENA <- readSource("IRENA")
  IRENA <- toolCountryFill(IRENA, fill = NA)
  IRENA[is.na(IRENA)] <- 0
  IRENACapacity <- IRENA[,,"Electricity capacity on grid"]
  # FossilFuels <- IRENACapacity[,,"Fossil fuels"]
  # FossilFuels <- collapseDim(FossilFuels, dim = c(3.2,3.3,3.4))
  # FossilFuels <- FossilFuels / 1000 # convert from MW to GW
  
  IRENAtoPGALL <- toolGetMapping(
    name = "IRENAtoPGALL.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IRENA, sep = ",") 
  
  map <- IRENAtoPGALL %>%
    mutate(IRENA = trimws(IRENA)) %>%
    filter(!is.na(IRENA), IRENA != "")
  
  IRENACapacity <- IRENACapacity[,,map[["IRENA"]]]
  IRENACapacity <- collapseDim(IRENACapacity, dim = c(3.2,3.3,3.4))
  IRENACapacity <- IRENACapacity / 1000 # convert from MW to GW
  
  IRENACapacity <- toolAggregate(IRENACapacity, dim = 3, rel = map, from = "IRENA", to = "PGALL", partrel = TRUE) 
  EmberCapacity <- getEmberCap()
  EmberCapacity <- collapseDim(EmberCapacity, dim = c(3.2))
  
  years <- intersect(getYears(IRENACapacity, as.integer = TRUE), getYears(EmberCapacity, as.integer = TRUE))
  IRENACapacity <- IRENACapacity[,years,] %>% as.quitte() %>%
    select(c("region", "variable", "period", "value"))
  EmberCapacity[is.na(EmberCapacity)] <- 0
  EmberCapacity <- EmberCapacity[,years,] %>% as.quitte() %>%
    select(c("region", "variable", "period", "value"))
  
  x <-   IRENACapacity %>%
    left_join(EmberCapacity, by = c("region", "variable", "period")) %>%
    mutate(value = ifelse(value.x == 0 & value.y != 0,
                          value.y,
                          value.x)) %>%
    select(-c("value.x", "value.y")) %>% as.quitte() %>% as.magpie()
  
  ########## HCL,LGN
  IDataElecProdFuel <- helperIDataElecProdFuel(mode = "Total")
  HCL_LGN <- dimSums(IDataElecProdFuel[,,c("HCL","LGN")], 3)
  HCL <- dimSums(IDataElecProdFuel[,,"HCL"],3) / HCL_LGN
  LGN <- dimSums(IDataElecProdFuel[,,"LGN"],3) / HCL_LGN
  LGN[is.na(LGN)] <- 0
  HCL[is.na(HCL)] <- 0
  #############
  ########## HYDRO
  hoursYear <- 8760
  capacitiesIDataElecProd <- calcOutput(type = "IDataElecProd", mode = "NonCHP", aggregate = FALSE) / hoursYear
  HYDRO <- dimSums(capacitiesIDataElecProd[,,c("PGLHYD","PGSHYD")], 3)
  PGLHYD <- dimSums(capacitiesIDataElecProd[,,"PGLHYD"],3) / HYDRO
  PGSHYD <- dimSums(capacitiesIDataElecProd[,,"PGSHYD"],3) / HYDRO
  PGLHYD[is.na(PGLHYD)] <- 0
  PGSHYD[is.na(PGSHYD)] <- 0
  #############
  
  x <- x[,getYears(capacitiesIDataElecProd),]
  
  xPGLHYD <- x[,,"PGLHYD"] * LGN
  xPGSHYD <- x[,,"PGLHYD"] * PGSHYD
  getItems(xPGSHYD, 3) <- "PGSHYD"
  
  xATHCOAL <- x[,,"ATHCOAL"] * HCL
  xATHLGN <- x[,,"ATHCOAL"] * LGN
  getItems(xATHLGN, 3) <- "ATHLGN"
  
  xWithoutDisag <- x[,,setdiff(getItems(x,3), c("PGLHYD", "PGSHYD", "ATHCOAL", "ATHLGN"))]
  xag <- mbind(xWithoutDisag, xPGLHYD, xPGSHYD, xATHCOAL, xATHLGN)
  
  ElecProdTotal <- helperIDataElecProdFuel(mode = "Total") %>% as.quitte() %>%
    select(region, period, ef, value) %>%
    filter(ef %in% c("HCL", "LGN", "GDO", "NGS", "BMSWAS"))
    
  ElecProdNonCHP <- helperIDataElecProdFuel(mode = "NonCHP") %>% as.quitte() %>%
    select(region, period, ef, value) %>%
    filter(ef %in% c("HCL", "LGN", "GDO", "NGS", "BMSWAS"))

  ElecProdCHP <- helperIDataElecProdFuel(mode = "CHP") %>% as.quitte() %>%
    select(region, period, ef, value) %>%
    filter(ef %in% c("HCL", "LGN", "GDO", "NGS", "BMSWAS"))
 
  ShareNonCHP <- ElecProdNonCHP %>% left_join(ElecProdTotal, by = c("region", "period", "ef")) %>% 
    mutate(share = value.x / value.y) %>% select(region, period, ef, share) %>% rename(value = share) %>% 
   as.quitte() %>%
   as.magpie()
 
  ShareCHP <- ElecProdCHP %>% left_join(ElecProdTotal, by = c("region", "period", "ef")) %>% 
   mutate(share = value.x / value.y) %>% select(region, period, ef, share) %>% rename(value = share) %>% 
   as.quitte() %>%
   as.magpie()

  ShareNonCHP[is.na(ShareNonCHP)] <- 0
  ShareCHP[is.na(ShareCHP)] <- 0
  
  PGALLtoEF <- toolGetMapping(
    name = "PGALLtoEF.csv",
    type = "blabla_export",
    where = "mrprom"
  )
  
  PGALLtoEF_no_ccs <- PGALLtoEF[!grepl("CCS", PGALLtoEF$PGALL), ]
  
  ShareNonCHP <- toolAggregate(ShareNonCHP, dim = 3, rel = PGALLtoEF_no_ccs, from = "EF", to = "PGALL", partrel = TRUE)
  ShareCHP <- toolAggregate(ShareCHP, dim = 3, rel = PGALLtoEF_no_ccs, from = "EF", to = "PGALL", partrel = TRUE)

  names(dimnames(ShareNonCHP))[3] <- names(dimnames(x))[3]
  names(dimnames(ShareCHP))[3] <- names(dimnames(x))[3]

  xagNonCHP <- xag[,,getItems(ShareNonCHP, 3)] * ShareNonCHP
  xagCHP <- xag[,,getItems(ShareCHP, 3)] * ShareCHP
  xagWithoutCHP <- xag[,,setdiff(getItems(xag,3), getItems(xagNonCHP,3))]
  
  fuel_map <- data.frame(
    TSTE = c("TSTE1AL", "TSTE1AH", "TSTE1AD", "TSTE1AG", "TSTE1AB"),
    PGALL = c("ATHLGN", "ATHCOAL", "ATHOIL", "ATHGAS", "ATHBMSWAS"),
    stringsAsFactors = FALSE
  )
  
  xagCHP <- toolAggregate(xagCHP, dim = 3, rel = fuel_map, from = "PGALL", to = "TSTE")
  
  xNonCHPfinal <- mbind(xagWithoutCHP, xagNonCHP)
  
  missingVar <- setdiff(PGALL[["PGALL"]], getItems(xNonCHPfinal, 3))
  
  xNonCHPfinal <- add_columns(xNonCHPfinal, addnm = missingVar, dim = 3, fill = 0)
  
  xfinal <- mbind(xNonCHPfinal, xagCHP)
  
  list(
    x = xfinal,
    weight = NULL,
    unit = "GW",
    description = "IRENA; Installed capacity"
  )
}


# Helper ------------------------------------------------
helperIDataElecProdFuel <- function(mode) {
  if (mode == "NonCHP") {
    subset <- c("ELMAINE", "ELAUTOE")
  } else if (mode == "CHP") {
    subset <- c("ELMAINC", "ELAUTOC")
  } else if (mode == "Total") {
    subset <- "ELOUTPUT"
  }
  
  fuelMap <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, EF = OPEN.PROM)
  
  data <- readSource("IEA2025", subset = subset) %>%
    as.quitte() %>%
    filter(value != 0, !is.na(value), unit == "GWH") %>%
    mutate(unit = "GWh") %>%
    select(-variable) %>%
    # map IEA products to OPEN-PROM EFs
    inner_join(fuelMap, by = "product") %>%
    # Aggregate to OPEN-PROM's EFs & SBS
    group_by(region, period, EF) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    as.quitte() %>%
    as.magpie()
  
  data[is.na(data)] <- 0
  
  suppressMessages(
    suppressWarnings(
      data <- toolCountryFill(data, fill = 0)
    )
  )
  
  return(data)
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



