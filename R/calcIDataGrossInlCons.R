#' calcIDataGrossInlCons
#'
#' Use data from ENERDATA to derive OPENPROM input parameter iDataGrossInlCons
#' This dataset includes data for gross inland consumption in Mtoe
#'
#' @return magpie object with OPENPROM input data iDataGrossInlCons
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataGrossInlCons", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join case_when if_else arrange
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom tibble deframe
#' @importFrom utils tail

calcIDataGrossInlCons <- function() {

  x <- calcOutput(type = "IDataOwnConsEne", aggregate = FALSE)
  
  # Loading necessary data sources
  # x <- readSource("ENERDATA", "electricity", convert = TRUE)
  # own_use <- readSource("ENERDATA", "own", convert = TRUE)
  # refineries <- readSource("ENERDATA", "input", convert = TRUE)
  # torf <- readSource("ENERDATA", "production", convert = TRUE)
  # bio <- readSource("ENERDATA", "biomass", convert = TRUE)
  indse <- calcOutput(type = "IFuelCons2", subtype = "INDSE", aggregate = FALSE)
  domse <- calcOutput(type = "IFuelCons2", subtype = "DOMSE", aggregate = FALSE)
  nense <- calcOutput(type = "IFuelCons2", subtype = "NENSE", aggregate = FALSE)
  transe <- calcOutput(type = "IFuelCons2", subtype = "TRANSE", aggregate = FALSE)

  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  # lastYear <- tail(sort(getYears(x, as.integer = TRUE)), 1)
  # x <- x[, c(fStartHorizon:lastYear), ]
  # own_use <- own_use[, c(fStartHorizon:lastYear), ]
  # refineries <- refineries[, c(fStartHorizon:lastYear), ]
  # torf <- torf[, c(fStartHorizon:lastYear), ]
  # bio <- bio[, c(fStartHorizon:lastYear), ]

  # load OPENPROM EFS set
  sets <- toolGetMapping(name = "EFS.csv",
                         type = "blabla_export",
                         where = "mrprom")
  
  sets <- as.character(sets[, 1])

  ## Only keep items with the Mtoe unit
  # x <- x[, , "Mtoe", pmatch = TRUE]
  # own_use <- own_use[, , "Mtoe", pmatch = TRUE]
  # refineries <- refineries[, , "Mtoe", pmatch = TRUE]
  # bio <- bio[, , "Mtoe", pmatch = TRUE]

  # Adding the PROM variables with placeholder values
  promnames <- sets[!(sets %in% c("MET", "ETH" ))]
  # for (name in promnames) {
  # 
  #   new_name <- paste0(name, ".Mtoe")
  #   x <- add_columns(x, addnm = name, dim = "variable", fill = 0.00000001)
  # }

  # Assigning the PROM variables that require calculations
  # x1 <- x[, , "Brown coal consumption of electricity sector.Mtoe"]
  # x2 <- own_use[, , "Lignite own use of energy industries.Mtoe"]
  # x[, , "LGN"] <- ifelse(is.na(x1), 0, x1) + ifelse(is.na(x2), 0, x2)
  
  # x3 <- x[, , "Coal consumption of electricity sector.Mtoe"]
  x4 <- x[, , "LGN"]
  
  years <- Reduce(intersect, list(getYears(indse), getYears(x)))
  indse <- indse[,years,]
  domse <- domse[,years,]
  nense <- nense[,years,]
  transe <- transe[,years,]
  
  x[, , "HCL"] <- rowSums(indse[, , "HCL", pmatch = TRUE], na.rm = TRUE, dims = 2) + rowSums(domse[, , "HCL", pmatch = TRUE], na.rm = TRUE, dims = 2) +
                  rowSums(nense[, , "HCL", pmatch = TRUE], na.rm = TRUE, dims = 2) +
                  + ifelse(is.na(x4), 0, x4)

  # x5 <- refineries[, , "Crude oil consumption of refineries input.Mtoe"]
  # x6 <- refineries[, , "NGL (natural gas liquids) refineries input.Mtoe"]
  # x7 <- own_use[, , "Crude oil own use of energy industries.Mtoe"]
  # x[, , "CRO"] <-  ifelse(is.na(x5), 0, x5) + ifelse(is.na(x6), 0, x6) + ifelse(is.na(x7), 0, x7)
                  
  # x8 <- own_use[, , "Motor gasoline own use of energy industries.Mtoe"]
  # x9 <- torf[, , "Motor gasoline production.Mtoe"] 
  # x10 <- transe[, , "PC.GSL"]
  # x[, , "GSL"] <- ifelse(is.na(x8), 0, x8) - ifelse(is.na(x9), 0, x9) +
  #                 ifelse(is.na(x10), 0, x10)

  x12 <- x[, , "GDO"]
  # x13 <- torf[, , "Diesel, heating oil production.Mtoe"]
  # x14 <- own_use[, , "Diesel, heating oil own use of energy industries.Mtoe"]
  # x15 <- transe[, , "PC.GDO"]
  # x16 <- transe[, , "PT.GDO"]
  # x17 <- transe[, , "GU.GDO"]
  # x18 <- transe[, , "GT.GDO"]
  x[, , "GDO"] <- rowSums(indse[, , "GDO", pmatch = TRUE], na.rm = TRUE, dims = 2) + rowSums(domse[, , "GDO", pmatch = TRUE], na.rm = TRUE, dims = 2) +
                  rowSums(nense[, , "GDO", pmatch = TRUE], na.rm = TRUE, dims = 2) +
                  ifelse(is.na(x12), 0, x12)

  x19 <- x[, , "RFO"]
  # x20 <- torf[, , "Heavy fuel oil production"]
  # x21<- own_use[, , "Heavy fuel oil own use of energy industries.Mtoe"]
  x[, , "RFO"] <- rowSums(domse[, , "RFO", pmatch = TRUE], na.rm = TRUE, dims = 2) + ifelse(is.na(x19), 0, x19)
  
  x22 <- x[, , "LPG"]
  # torf[, , "LPG (including ethane before 1990) production"]
  # x23 <- own_use[, , "LPG (liquified petroleum gas) own use of energy industries.Mtoe"]
  # x24 <- transe[, , "PC.LPG"]
  x[, , "LPG"] <- rowSums(domse[, , "LPG", pmatch = TRUE], na.rm = TRUE, dims = 2) + rowSums(nense[, , "LPG", pmatch = TRUE], na.rm = TRUE, dims = 2) + ifelse(is.na(x22), 0, x22)
                  
  # x25 <- transe[, ,"PA.KRS"]
  # x26 <- torf[, , "Kerosene production"]
  # x[, , "KRS"] <- ifelse(is.na(x25), 0, x25) - ifelse(is.na(x26), 0, x26)

  # x27 <- own_use[, , "Oil products own use of energy industries.Mtoe"]
  # x28 <- own_use[, , "Motor gasoline own use of energy industries.Mtoe"]
  # x29 <- own_use[, , "Diesel, heating oil own use of energy industries.Mtoe"]
  # x30 <- own_use[, , "Heavy fuel oil own use of energy industries.Mtoe"]
  # x31 <- own_use[, , "LPG (liquified petroleum gas) own use of energy industries.Mtoe"]
  # x32 <- own_use[, , "Kerosene own use of energy industries.Mtoe"]
  # x33 <- torf[, , "Oil products production"]
  # x34 <- torf[, , "Motor gasoline production.Mtoe"]
  # x35 <- torf[, , "Diesel, heating oil production.Mtoe"]
  # x36 <- torf[, , "Heavy fuel oil production"]
  # x37 <- torf[, , "LPG (including ethane before 1990) production"]
  # x38 <- torf[, , "Kerosene production"]
  # 
  # x[, , "OLQ"] <- ( ifelse(is.na(x27), 0, x27) - ( ifelse(is.na(x28), 0, x28) + ifelse(is.na(x29), 0, x29) +
  #                   ifelse(is.na(x30), 0, x30) + ifelse(is.na(x31), 0, x31) +
  #                   ifelse(is.na(x32), 0, x32))) - (ifelse(is.na(x33), 0, x33) -
  #                   ifelse(is.na(x34), 0, x34) - ifelse(is.na(x35), 0, x35) -
  #                   ifelse(is.na(x36), 0, x36) - ifelse(is.na(x37), 0, x37) - ifelse(is.na(x38), 0, x38) )
         
  x39 <- x[, , "NGS"]
  #   x[, , "Natural gas input in electricity power plants.Mtoe"]
  # x40 <- own_use[, , "Natural gas own use of energy industries.Mtoe"]
  # x41 <- transe[, , "PC.NGS"]
  # x50 <- transe[, , "GU.NGS"]
  x[, , "NGS"] <- rowSums(indse[, , "NGS", pmatch = TRUE], na.rm = TRUE, dims = 2) + rowSums(domse[, , "NGS", pmatch = TRUE], na.rm = TRUE, dims = 2) +
                  rowSums(nense[, , "NGS", pmatch = TRUE], na.rm = TRUE, dims = 2) +
                  ifelse(is.na(x39), 0, x39) 
  # + ifelse(is.na(x40), 0, x40) + ifelse(is.na(x41), 0, x41) + ifelse(is.na(x50), 0, x50)
                  
  x42 <- x[, , "OGS"]
  #   x[, , "Gas consumption of electricity sector.Mtoe"]
  # x43 <- x[, , "Natural gas input in electricity power plants"]
  # x44 <- own_use[, , "Gas own use of energy industries.Mtoe"]
  # x45 <- own_use[, , "Natural gas own use of energy industries.Mtoe"]
  x[, , "OGS"] <- rowSums(indse[, , "OGS", pmatch = TRUE], na.rm = TRUE, dims = 2) +
                  (ifelse(is.na(x42), 0, x42)) 
                   # - ifelse(is.na(x43), 0, x43)) +
                  # (ifelse(is.na(x44), 0, x44) - ifelse(is.na(x45), 0, x45))

  # x[, , "NUC"] <- torf[, , "Nuclear electricity production.GWh"] / 1000 * 0.086 # Converting GWh to Mtoe
  # 
  # x[, , "HYD"] <- torf[, , "Hydroelectric production.GWh"] / 1000 * 0.086
  # 
  # x[, , "BMSWAS"] <- bio[, , "Primary consumption of biomass.Mtoe"]
  # 
  # x[, , "WND"] <- torf[, , "Wind electricity production.GWh"] / 1000 * 0.086
  # 
  # x[, , "SOL"] <- torf[, , "Solar electricity production.GWh"] / 1000 * 0.086
  # 
  # x[, , "GEO"] <- torf[, , "Geothermal electricity production.GWh"] / 1000 * 0.086

  x46 <- x[, , "ELC"]
  # x47 <- own_use[, , "Electricity own use of energy industries.Mtoe"]
  # x48 <- transe[, , "PT.ELC"]
  # x49 <- transe[, , "GT.ELC"]
  x[, , "ELC"] <- (rowSums(transe[, , "ELC", pmatch = TRUE], na.rm = TRUE, dims = 2) + 
                     ifelse(is.na(x46), 0, x46)) 
  # + ifelse(is.na(x47), 0, x47) +
  #                  ifelse(is.na(x48), 0, x48) + ifelse(is.na(x49), 0, x49)

  # Only keeping the PROM variables and dropping the rest        
  x <- x[, , promnames]
  fill_years <- getYears(x, as.integer = TRUE)
  
  x <- as.quitte(x)
  x <- select(x,-variable)
  names(x) <- sub("efs", "variable", names(x))

  # Converting to quitte object and interpolating periods
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = fill_years, expand.values = TRUE)
  qx_bu <- qx

  # Assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  names(qx) <- sub("region", "CountryCode", names(qx))

  ## Add h12 mapping to dataset
  qx <- left_join(qx, h12, by = "CountryCode")

  ## Add new column containing regional mean value
  value <- NULL
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "variable"))
  names(qx) <- sub("CountryCode", "region", names(qx))
  qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  qx_bu <- select(qx_bu, -c("model", "scenario"))

  ## Assign the H12 region mean where necessary
  value.x <- NULL
  value.y <- NULL
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))

  ## Assign to countries that still have NA, the global mean
  qx_bu <- qx
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "variable"))
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))

  # Converting to magpie object
  x <- as.quitte(qx) %>% as.magpie()
  # Set NA to 0
  x[is.na(x)] <- 0
  list(x = x,
       weight = NULL,
       unit = "Mtoe",
       description = "Enerdata; Gross Inland Consumption")
}
