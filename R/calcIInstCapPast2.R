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
  
  fStartHorizon <- readEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fStartHorizon"]
  
  IRENA <- readSource("IRENA")
  IRENA <- toolCountryFill(IRENA, fill = NA)
  IRENA[is.na(IRENA)] <- 0
  IRENACapacity <- IRENA[,,"Electricity capacity on grid"]
  FossilFuels <- IRENACapacity[,,"Fossil fuels"]
  FossilFuels <- collapseDim(FossilFuels, dim = c(3.2,3.3,3.4))
  FossilFuels <- FossilFuels / 1000 # convert from MW to GW
  
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
  
  capacities <- toolAggregate(IRENACapacity, dim = 3, rel = map, from = "IRENA", to = "PGALL", partrel = TRUE)
  
  hoursYear <- 8760
  capacitiesIDataElecProd <- calcOutput(type = "IDataElecProd", mode = "NonCHP", aggregate = FALSE) / hoursYear
  capacitiesIDataElecProd <- capacitiesIDataElecProd[,,c("ATHCOAL", "ATHGAS", "ATHOIL")]
  capacitiesIDataElecProdSum <- dimSums(capacitiesIDataElecProd, dim = 3)
  capacitiesIDataElecProdSum <- capacitiesIDataElecProdSum[,2023,]
  capacitiesIDataElecProdSum <- collapseDim(capacitiesIDataElecProdSum, dim = 2)
  capacitiesIDataElecProdShare <- capacitiesIDataElecProd / capacitiesIDataElecProdSum
  capacitiesIDataElecProdShare <- capacitiesIDataElecProdShare[,2023,]
  capacitiesIDataElecProdShare <- collapseDim(capacitiesIDataElecProdShare, dim = 2)
  
  FossilFuels2 <- collapseDim(FossilFuels, dim = c(3))
  FossilFuelsTotal <- FossilFuels2 * capacitiesIDataElecProdShare
  
  qcapacities <- as.quitte(capacities)
  qFossilFuelsTotal <- as.quitte(FossilFuelsTotal)
  qFossilFuels <- as.quitte(FossilFuels)
  
  qFossilFuelsFull <- rbind(qFossilFuelsTotal, qFossilFuels) 
  
  final <- full_join(qcapacities, qFossilFuelsFull, by = c("region", "model", "scenario",
                                                                    "variable", "unit", "period"))
  
  x <- final %>%
    group_by(region, period) %>%
    mutate(
      all_zero = all(
        value.x[variable %in% c("ATHCOAL", "ATHGAS", "ATHOIL")] == 0,
        na.rm = TRUE
      ),
      fossil_y_exists = any(
        variable == "Fossil fuels" & !is.na(value.y)
      ),
      value.x = if_else(
        variable %in% c("ATHCOAL", "ATHGAS", "ATHOIL") &
          all_zero &
          fossil_y_exists &
          !is.na(value.y),
        value.y,
        value.x
      )
    ) %>%
    ungroup() %>%
    select(-all_zero, -fossil_y_exists, -value.y) %>%
    rename(value = value.x) %>%
    filter(
      variable != "Fossil fuels"
      )
  
  x <- as.quitte(x) %>% as.magpie()

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
  
  ShareNonCHP <- toolAggregate(ShareNonCHP, dim = 3, rel = PGALLtoEF, from = "EF", to = "PGALL", partrel = TRUE)
  ShareCHP <- toolAggregate(ShareCHP, dim = 3, rel = PGALLtoEF, from = "EF", to = "PGALL", partrel = TRUE)

  names(dimnames(ShareNonCHP))[3] <- names(dimnames(x))[3]
  names(dimnames(ShareCHP))[3] <- names(dimnames(x))[3]

  ########## HCL,LGN
  IDataElecProdFuel <- helperIDataElecProdFuel(mode = "Total")
  HCL_LGN <- dimSums(IDataElecProdFuel[,,c("HCL","LGN")], 3)
  HCL <- dimSums(IDataElecProdFuel[,,"HCL"],3) / HCL_LGN
  LGN <- dimSums(IDataElecProdFuel[,,"LGN"],3) / HCL_LGN
  LGN[is.na(LGN)] <- 0
  HCL[is.na(HCL)] <- 0
  #############
  ########## HYDRO
  HYDRO <- calcOutput(type = "IDataElecProd", mode = "NonCHP", aggregate = FALSE)
  HCL_LGN <- dimSums(IDataElecProdFuel[,,c("HCL","LGN")], 3)
  HCL <- dimSums(IDataElecProdFuel[,,"HCL"],3) / HYDRO
  LGN <- dimSums(IDataElecProdFuel[,,"LGN"],3) / HYDRO
  LGN[is.na(LGN)] <- 0
  HCL[is.na(HCL)] <- 0
  #############

  x <- x[,getItems(ShareNonCHP, 2),]
  total <- x[,,setdiff(getItems(x, 3), getItems(ShareCHP, 3))]
  LGNx <- x[,, "ATHCOAL"] * LGN
  getItems(LGNx, 3) <- "ATHLGN"
  HCLx <- x[,, "ATHCOAL"] * HCL
  totalx <- mbind(total, LGNx, HCLx)

  missingVar <- setdiff(IRENAtoPGALL[["PGALL"]], getItems(x, 3))
  
  x <- add_columns(x, addnm = missingVar, dim = 3, fill = 0)
  
  
  list(
    x = x,
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
    filter(EF %in% c("HCL", "LGN", "GDO", "NGS", "BMSWAS")) %>% 
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


