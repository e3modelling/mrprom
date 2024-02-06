#' calcIDataGrossInlCons
#'
#' Use data from ENERDATA to derive OPENPROM input parameter iDataGrossInlCons
#' This dataset includes data for gross inland consumption in Mtoe.
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

  # Loading necessary data sources
  x <- readSource("ENERDATA", "electricity", convert = TRUE)
  own_use <- readSource("ENERDATA", "own", convert = TRUE)
  refineries <- readSource("ENERDATA", "input", convert = TRUE)
  torf <- readSource("ENERDATA", "production", convert = TRUE)
  bio <- readSource("ENERDATA", "biomass", convert = TRUE)
  indse <- calcOutput(type = "IFuelCons", subtype = "INDSE", aggregate = FALSE)
  domse <- calcOutput(type = "IFuelCons", subtype = "DOMSE", aggregate = FALSE)
  nense <- calcOutput(type = "IFuelCons", subtype = "NENSE", aggregate = FALSE)
  transe <- calcOutput(type = "IFuelCons", subtype = "TRANSE", aggregate = FALSE)

  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  lastYear <- tail(sort(getYears(x, as.integer = TRUE)), 1)
  x <- x[, c(fStartHorizon:lastYear), ]
  own_use <- own_use[, c(fStartHorizon:lastYear), ]
  refineries <- refineries[, c(fStartHorizon:lastYear), ]
  torf <- torf[, c(fStartHorizon:lastYear), ]
  bio <- bio[, c(fStartHorizon:lastYear), ]

  # Interpolate missing periods for TRANSE dataset
  transe <- as.quitte(transe) %>%
    interpolate_missing_periods(period = getYears(x, as.integer = TRUE), expand.values = TRUE) %>%
    as.magpie()

  # load OPENPROM EFS set
  sets <- readSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "EFS")
  sets <- unlist(strsplit(sets[, 1], ","))

  ## Only keep items with the Mtoe unit
  x <- x[, , "Mtoe", pmatch = TRUE]
  own_use <- own_use[, , "Mtoe", pmatch = TRUE]
  refineries <- refineries[, , "Mtoe", pmatch = TRUE]
  bio <- bio[, , "Mtoe", pmatch = TRUE]

  # Adding the PROM variables with placeholder values
  promnames <- sets[1:19]
  for (name in promnames) {

    new_name <- paste0(name, ".Mtoe")
    x <- add_columns(x, addnm = name, dim = "variable", fill = 0.00000001)
  }

  # Assigning the PROM variables that require calculations
  x[, , "LGN"] <- x[, , "Brown coal consumption of electricity sector.Mtoe"] + own_use[, , "Lignite own use of energy industries.Mtoe"]

  x[, , "HCL"] <- rowSums(indse[, , "HCL", pmatch = TRUE], dims = 2) + rowSums(domse[, , "HCL", pmatch = TRUE], dims = 2) +
                  rowSums(nense[, , "HCL", pmatch = TRUE], dims = 2) +
                  x[, , "Coal consumption of electricity sector.Mtoe"] + own_use[, , "Coal own use of energy industries.Mtoe"]

  x[, , "CRO"] <- refineries[, , "Crude oil consumption of refineries input.Mtoe"] + refineries[, , "NGL (natural gas liquids) refineries input.Mtoe"] +
                  own_use[, , "Crude oil own use of energy industries.Mtoe"]

  x[, , "GSL"] <- own_use[, , "Motor gasoline own use of energy industries.Mtoe"] - torf[, , "Motor gasoline production.Mtoe"] +
                  transe[, , "PC.Mtoe.GSL"] + transe[, , "GU.Mtoe.GSL"]

  x[, , "GDO"] <- rowSums(indse[, , "GDO", pmatch = TRUE], dims = 2) + rowSums(domse[, , "GDO", pmatch = TRUE], dims = 2) +
                  rowSums(nense[, , "GDO", pmatch = TRUE], dims = 2) +
                  x[, , "Diesel, heating oil input in electricity power plants.Mtoe"] - torf[, , "Diesel, heating oil production.Mtoe"] +
                  own_use[, , "Diesel, heating oil own use of energy industries.Mtoe"] +
                  transe[, , "PC.Mtoe.GDO"] + transe[, , "PT.Mtoe.GDO"] + transe[, , "GU.Mtoe.GDO"] + transe[, , "GT.Mtoe.GDO"]

  x[, , "RFO"] <- rowSums(domse[, , "RFO", pmatch = TRUE], dims = 2) + x[, , "Heavy fuel oil input in electricity power plants.Mtoe"] -
                  torf[, , "Heavy fuel oil production"] + own_use[, , "Heavy fuel oil own use of energy industries.Mtoe"]

  x[, , "LPG"] <- rowSums(domse[, , "LPG", pmatch = TRUE], dims = 2) + rowSums(nense[, , "LPG", pmatch = TRUE], dims = 2) -
                  torf[, , "LPG (including ethane before 1990) production"] + own_use[, , "LPG (liquified petroleum gas) own use of energy industries.Mtoe"] +
                  transe[, , "PC.Mtoe.LPG"]

  x[, , "KRS"] <- transe[, ,"PA.Mtoe.KRS"] - torf[, , "Kerosene production"]

  x[, , "OLQ"] <- (own_use[, , "Oil products own use of energy industries.Mtoe"] -
                  (own_use[, , "Motor gasoline own use of energy industries.Mtoe"] + own_use[, , "Diesel, heating oil own use of energy industries.Mtoe"] +
                   own_use[, , "Heavy fuel oil own use of energy industries.Mtoe"] + own_use[, , "LPG (liquified petroleum gas) own use of energy industries.Mtoe"] +
                   own_use[, , "Kerosene own use of energy industries.Mtoe"])) -
                  (torf[, , "Oil products production"] - torf[, , "Motor gasoline production.Mtoe"] - torf[, , "Diesel, heating oil production.Mtoe"] -
                   torf[, , "Heavy fuel oil production"] - torf[, , "LPG (including ethane before 1990) production"] - torf[, , "Kerosene production"])
         

  x[, , "NGS"] <- rowSums(indse[, , "NGS", pmatch = TRUE], dims = 2) + rowSums(domse[, , "NGS", pmatch = TRUE], dims = 2) +
                  rowSums(nense[, , "NGS", pmatch = TRUE], dims = 2) +
                  x[, , "Natural gas input in electricity power plants.Mtoe"] + own_use[, , "Natural gas own use of energy industries.Mtoe"] +
                  transe[, , "PC.Mtoe.NGS"] + transe[, , "GU.Mtoe.NGS"]

  x[, , "OGS"] <- rowSums(indse[, , "OGS", pmatch = TRUE], dims = 2) +
                  (x[, , "Gas consumption of electricity sector.Mtoe"] - x[, , "Natural gas input in electricity power plants"]) +
                  (own_use[, , "Gas own use of energy industries.Mtoe"] - own_use[, , "Natural gas own use of energy industries.Mtoe"])

  x[, , "NUC"] <- torf[, , "Nuclear electricity production.GWh"] / 1000 * 0.086 # Converting GWh to Mtoe

  x[, , "HYD"] <- torf[, , "Hydroelectric production.GWh"] / 1000 * 0.086

  x[, , "BMSWAS"] <- bio[, , "Primary consumption of biomass.Mtoe"]

  x[, , "WND"] <- torf[, , "Wind electricity production.GWh"] / 1000 * 0.086

  x[, , "SOL"] <- torf[, , "Solar electricity production.GWh"] / 1000 * 0.086

  x[, , "GEO"] <- torf[, , "Geothermal electricity production.GWh"] / 1000 * 0.086

  x[, , "ELC"] <- (torf[, , "Electricity production.GWh"] / 1000 * 0.086) + own_use[, , "Electricity own use of energy industries.Mtoe"] +
                  transe[, , "PT.Mtoe.ELC"] + transe[, , "GT.Mtoe.ELC"]


  # Only keeping the PROM variables and dropping the rest        
  x <- x[, , promnames]

  # Converting to quitte object and interpolating periods
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = getYears(x, as.integer = TRUE), expand.values = TRUE)
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
