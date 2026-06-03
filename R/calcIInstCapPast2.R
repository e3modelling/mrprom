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
  
  missingVar <- setdiff(IRENAtoPGALL[["PGALL"]], getItems(x, 3))
  
  x <- add_columns(x, addnm = missingVar, dim = 3, fill = 0)
  
  list(
    x = x,
    weight = NULL,
    unit = "GW",
    description = "IRENA; Installed capacity"
  )
}

