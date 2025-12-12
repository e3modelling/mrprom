#' fullOPEN-PROM
#'
#' Read in several files with data from GEME3, IRF and WDI_PA and convert it
#' to a csv file.
#' The dataset contains several data types about transport, traffic, air
#' transport passengers per country and per year and Production Level and
#' Unit Cost data.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @importFrom dplyr %>% select left_join mutate filter distinct
#' @importFrom tidyr pivot_wider expand nesting
#' @importFrom stringr str_replace
#' @importFrom quitte as.quitte
#' @importFrom utils write.table
#'
#' @examples
#' \dontrun{
#' a <- retrieveData("OPEN_PROM", regionmapping = "regionmappingOP.csv")
#' }
fullOPEN_PROM <- function() {
  # compute weights for aggregation by population
  map <- toolGetMapping(getConfig("regionmapping"), "regional", where = "mrprom")

  # population
  population <- calcOutput(type = "POP", aggregate = FALSE)
  population <- as.quitte(population)

  names(population) <- sub("region", "ISO3.Code", names(population))

  ## add mapping to population
  population <- left_join(population, map, by = "ISO3.Code")
  weights <- NULL
  value <- NULL
  period <- NULL
  # take the sum of population of each region
  POP <- mutate(population, weights = sum(value, na.rm = TRUE), .by = c("Region.Code", "period"))
  # compute weights by deviding the population of country with the sum of region
  POP["weights"] <- POP["value"] / POP["weights"]
  # select period 2020 for the weights
  POP <- POP %>% filter(period == 2020)
  POP <- POP %>% select(c("ISO3.Code", "weights"))
  names(POP) <- sub("ISO3.Code", "region", names(POP))
  names(POP) <- sub("weights", "value", names(POP))
  POP <- as.magpie(as.quitte(POP))
  POP <- collapseDim(POP, dim = 3.1)

  suppressWarnings({
    x <- calcOutput("ACTV", aggregate = FALSE)
    transport <- calcOutput("ACTV", aggregate = TRUE)
  })
  transport <- transport[, , setdiff(getItems(transport, 3.2), "%")]
  x <- x[, , "%"]
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  x <- mbind(x, transport)
  xq <- as.quitte(x) %>%
    select(c("period", "region", "value", "variable")) %>%
    pivot_wider(names_from = "variable")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iACTV.csvr")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iACTV.csvr",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput("POP", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("period", "region", "value")) %>%
    pivot_wider(names_from = "region")
  fheader <- paste("dummy", paste(colnames(xq)[2:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iPop.csvr")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iPop.csvr",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  stockPC <- calcOutput("StockPC", aggregate = FALSE)
  xq <- toolAggregate(stockPC, weight = NULL, rel = map, from = "ISO3.Code", to = "Region.Code") %>%
    as.quitte() %>%
    select(c("period", "region", "tech", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iStockPC.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iStockPC.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  stockPC[stockPC == 0] <- 1e-6
  stockPC <- add_columns(stockPC, addnm = paste0("y", seq(2021, 2100)), dim = 2)
  stockPC[, paste0("y", seq(2021, 2100)), ] <- stockPC[, "y2020", ]
  xq <- calcOutput("ISFC", subtype = "projection", aggregate = FALSE) %>%
    toolAggregate(weight = stockPC, dim = 1, rel = map, from = "ISO3.Code", to = "Region.Code") %>%
    as.quitte() %>%
    select(c("region", "period", "tech", "fuel", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy,dummy", paste(colnames(xq)[4:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iSFC.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iSFC.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput("iGDP", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("period", "region", "value")) %>%
    pivot_wider(names_from = "region")
  fheader <- paste("dummy", paste(colnames(xq)[2:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iGDP.csvr")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iGDP.csvr",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IFuelCons2", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("period", "value", "region", "dsbs", "ef")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy,dummy", paste(colnames(xq)[4:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = paste0("iFuelCons.csv"))
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = paste0("iFuelCons.csv"),
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput("IFuelPrice", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  # write input data file that GAMS can read
  xq <- as.quitte(x)
  xq <- xq[!is.na(xq[["value"]]), ] %>%
    select(c("period", "value", "region", "variable", "new")) %>% # nolint
    pivot_wider(names_from = "period") # nolint
  fheader <- paste("dummy,dummy,dummy", paste(colnames(xq)[4:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iFuelPrice.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iFuelPrice.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput("INewReg", aggregate = TRUE)
  # write input data file that GAMS can read
  xq <- as.quitte(x) %>%
    select(c("period", "value", "region")) %>% # nolint
    pivot_wider(names_from = "period") # nolint
  fheader <- paste("dummy", paste(colnames(xq)[2:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iNewReg.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iNewReg.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )
  
  suppressWarnings({
    x <- calcOutput("ITransChar", aggregate = FALSE)
  })
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  # write input data file that GAMS can read
  xq <- as.quitte(x)
  xq <- xq[!is.na(xq[["value"]]), ] %>%
    select(c("period", "value", "region", "variable")) %>% # nolint
    pivot_wider(names_from = "period") # nolint
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iTransChar.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iTransChar.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IDataPassCars", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  a <- as.quitte(x)
  z <- select(a, "region", "unit", "period", "value")
  z <- pivot_wider(z, names_from = "period", values_from = "value")
  fheader <- paste("dummy,dummy,scr")
  writeLines(fheader, con = paste0("iDataPassCars", ".csv"))
  write.table(z[, c(1, 2, 5)],
    quote = FALSE,
    row.names = FALSE,
    file = paste0("iDataPassCars", ".csv"),
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput("IDataElecSteamGen", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("period", "value", "region", "variable")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataElecSteamGen.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iDataElecSteamGen.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput("IDataDistrLosses", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataDistrLosses.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iDataDistrLosses.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput("IDataOwnConsEne", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "period", "sector", "efs", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("region,sector,efs", paste(colnames(xq)[4:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataOwnConsEne.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iDataOwnConsEne.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput("IRatioBranchOwnCons", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "period", "sector", "variable", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("region,sector,variable", paste(colnames(xq)[4:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iRatioBranchOwnCons.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iRatioBranchOwnCons.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput("IDataImports", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataImports.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iDataImports.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput("ISuppExports", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iSuppExports.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iSuppExports.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput("IDataTransTech", aggregate = FALSE)
  x <- as.quitte(x)
  x <- select(x, c("transfinal", "ttech", "value", "variable", "period")) %>%
    pivot_wider(names_from = "period")
  xq <- x
  fheader <- paste("dummy,dummy,dummy", paste(colnames(xq)[4:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataTransTech.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iDataTransTech.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  suppressWarnings({
    x <- calcOutput("IEnvPolicies", aggregate = FALSE)
  })
  # POP is weights for aggregation, perform aggregation
  weight_EMI_CO2 <- readSource("PIK", convert = TRUE)
  weight_EMI_CO2 <- weight_EMI_CO2[, 2020, "Energy.MtCO2.CO2"]
  weight_EMI_CO2[is.na(weight_EMI_CO2)] <- 0
  x <- toolAggregate(x, weight = weight_EMI_CO2, rel = map, from = "ISO3.Code", to = "Region.Code")
  xq <- as.quitte(x) %>%
    select(c("region", "policies_set", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iEnvPolicies.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iEnvPolicies.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IDataElecProd", mode = "NonCHP", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataElecProdNonCHP.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iDataElecProdNonCHP.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IDataElecProd", mode = "CHP", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "ef", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataElecProdCHP.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iDataElecProdCHP.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IVarCost", aggregate = FALSE)
  xq <- as.quitte(x) %>%
    select(c("variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy", paste(colnames(xq)[2:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iVarCost.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iVarCost.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IDataPlantEffByType", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataPlantEffByType.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iDataPlantEffByType.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IDataTechLftPlaType", aggregate = FALSE)
  xq <- as.quitte(x) %>%
    select(c("variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy", "LFT", sep = ",")
  writeLines(fheader, con = "iDataTechLftPlaType.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iDataTechLftPlaType.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IAvailRate", aggregate = FALSE)
  w <- x
  w[, ] <- ifelse(x[, ] == 0, 0, 1)
  x <- toolAggregate(x,
    weight = w, zeroWeight = "allow",
    rel = map, from = "ISO3.Code", to = "Region.Code"
  )
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iAvailRate.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iAvailRate.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IChpPowGen", aggregate = FALSE)
  xq <- as.quitte(x) %>%
    select(c("technology", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy", "dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iChpPowGen.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iChpPowGen.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IFixOandMCost", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iFixOandMCost.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iFixOandMCost.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IGrossCapCosSubRen", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "IGrossCapCosSubRen.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "IGrossCapCosSubRen.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput("IInstCapPast", mode = "NonCHP", aggregate = TRUE)
  variable <- NULL
  xq <- as.quitte(x) %>%
    filter(variable != "PGNUC") %>%
    select(c("period", "value", "region", "variable")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iInstCapPastNonCHP.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iInstCapPastNonCHP.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput("IInstCapPast", mode = "CHP", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    filter(variable != "PGNUC") %>%
    select(c("period", "value", "region", "ef")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iInstCapPastCHP.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iInstCapPastCHP.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "ITotEneSupply", subtype = "Primary", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iPrimProd.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iPrimProd.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  for (z in c("Inp", "Out")) {
    x <- calcOutput(type = "ITransfProcess", flow = z, aggregate = TRUE)
    xq <- as.quitte(x) %>%
      select(c("region", "period", "sector", "variable", "value")) %>%
      pivot_wider(names_from = "period")
    fheader <- paste("dummy,dummy,dummy", paste(colnames(xq)[4:length(colnames(xq))], collapse = ","), sep = ",")
    writeLines(fheader, con = paste0("i", z, "TransfProcess.csv"))
    write.table(xq,
      quote = FALSE,
      row.names = FALSE,
      file = paste0("i", z, "TransfProcess.csv"),
      sep = ",",
      col.names = FALSE,
      append = TRUE
    )
  }

  x <- calcOutput(type = "IDataElecInd", aggregate = TRUE) %>%
    as.quitte() %>%
    select(c("region", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy", paste(colnames(x)[2:length(colnames(x))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataElecInd.csv")
  write.table(x,
    quote = FALSE,
    row.names = FALSE,
    file = "iDataElecInd.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "ISuppRatePrimProd", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iSuppRatePrimProd.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iSuppRatePrimProd.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "ISuppRefCapacity", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iSuppRefCapacity.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iSuppRefCapacity.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IElcNetImpShare", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  # write input data file that GAMS can read
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iElcNetImpShare.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iElcNetImpShare.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IDataGrossInlCons", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataGrossInlCons.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iDataGrossInlCons.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IMatFacPlaAvailCap", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iMatFacPlaAvailCap.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iMatFacPlaAvailCap.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IInvPlants", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iInvPlants.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iInvPlants.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IDecomPlants", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDecomPlants.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iDecomPlants.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IPriceFuelsInt", aggregate = FALSE)
  xq <- as.quitte(x) %>%
    select(c("variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy", paste(colnames(xq)[2:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iPriceFuelsInt.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iPriceFuelsInt.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "INatGasPriProElst", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  suppressWarnings({
    xq <- as.quitte(x) %>% select(c("region", "value"))
  })
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iNatGasPriProElst.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IPriceFuelsIntBase", aggregate = FALSE)
  xq <- as.quitte(x) %>%
    select(c("variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy", paste(colnames(xq)[2:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iPriceFuelsIntBase.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iPriceFuelsIntBase.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IDataTotTransfInputRef", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataTotTransfInputRef.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iDataTotTransfInputRef.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "ISuppTransfers", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iSuppTransfers.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iSuppTransfers.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "UNFCCC_IP_CO2", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iUNFCCC_IP_CO2.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iUNFCCC_IP_CO2.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput(type = "IMatureFacPlaDisp", aggregate = FALSE)
  xq <- as.quitte(x)
  names(xq) <- sub("region", "ISO3.Code", names(xq))
  xq <- left_join(xq, map, by = "ISO3.Code")
  for (i in unique(xq[["Region.Code"]])) {
    if (sum(xq[which(xq[["variable"]] == "PGAWNO" & xq[["Region.Code"]] == i), 7]) > 0.5) {
      xq[which(xq[["variable"]] == "PGAWNO" & xq[["Region.Code"]] == i), 7] <- 0.6
    }
  }
  xq <- xq %>% select(c("Region.Code", "variable", "period", "value"))
  xq <- distinct(xq)
  xq <- xq %>% pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iMatureFacPlaDisp.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iMatureFacPlaDisp.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput("IH2Production", aggregate = FALSE)
  x <- as.quitte(x) %>%
    select(c("variable", "h2ttech", "period", "value"))
  x <- x %>% pivot_wider(names_from = "period")
  xq <- x
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iH2Production.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iH2Production.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput("IH2InfrCapCosts", aggregate = FALSE)
  x <- as.quitte(x) %>%
    select(c("variable", "infrastructure", "period", "value"))
  x <- x %>% pivot_wider(names_from = "period")
  xq <- x
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iH2InfrCapCosts.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iH2InfrCapCosts.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  x <- calcOutput("IH2Parameters", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  suppressWarnings({
    x <- as.quitte(x) %>% select(c("region", "parameters", "value"))
  })
  xq <- x %>% pivot_wider(names_from = "parameters", values_from = "value")
  fheader <- paste("dummy", paste(colnames(xq)[2:length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iH2Parameters.csv")
  write.table(xq,
    quote = FALSE,
    row.names = FALSE,
    file = "iH2Parameters.csv",
    sep = ",",
    col.names = FALSE,
    append = TRUE
  )

  return(list(
    x = x,
    weight = NULL,
    unit = "various",
    description = "OPENPROM input data"
  ))
}
