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
#' @importFrom dplyr %>% select left_join mutate filter
#' @importFrom tidyr pivot_wider expand nesting
#' @importFrom stringr str_replace
#' @importFrom quitte as.quitte
#' @importFrom utils write.table
#'
#' @examples
#' \dontrun{
#'  a <- retrieveData("OPEN_PROM", regionmapping = "regionmappingOP.csv")
#' }

fullOPEN_PROM <- function() {
  
  # compute weights for aggregation by population
  map <- toolGetMapping(getConfig("regionmapping"), "regional", where = "mrprom")
  
  # population
  population <- calcOutput(type = "POP", aggregate = FALSE)
  population <- as.quitte(population)
  
  # compute weights by population
  names(population) <- sub("region", "ISO3.Code", names(population))
  
  ## add mapping to population
  population <- left_join(population, map, by = "ISO3.Code")
  weights <- NULL
  value <- NULL
  period <- NULL
  POP <- mutate(population, weights = sum(value, na.rm = TRUE), .by = c("Region.Code", "period"))
  POP["weights"] <- POP["value"] / POP["weights"]
  POP <- POP %>% filter(period == 2020)
  POP <- POP %>% select(c("ISO3.Code", "weights")) 
  names(POP) <- sub("ISO3.Code", "region", names(POP))
  names(POP) <- sub("weights", "value", names(POP))
  POP <- as.magpie(as.quitte(POP))
  POP <- collapseDim(POP,dim = 3.1)
  
  calcOutput(type = "ACTV", file = "iACTV.csvr", aggregate = TRUE)
  calcOutput(type = "POP", file = "iPop.csvr", aggregate = TRUE)
  calcOutput(type = "iGDP", file = "iGDP.csvr", aggregate = TRUE)

  for (i in c("NENSE", "DOMSE", "INDSE", "TRANSE")) {
    x <- calcOutput(type = "Navigate", subtype = i, aggregate = TRUE)
    x[is.na(x)] <- 0
    xq <- as.quitte(x) %>%
          select(c("period", "value", "region", "variable", "new")) %>% # nolint
          pivot_wider(names_from = "period") # nolint
    fheader <- paste("dummy,dummy,dummy", paste(colnames(xq)[4 : length(colnames(xq))], collapse = ","), sep = ",")
    writeLines(fheader, con = paste0("iFuelCons", i, ".csv"))
    write.table(xq,
                quote = FALSE,
                row.names = FALSE,
                file = paste0("iFuelCons", i, ".csv"),
                sep = ",",
                col.names = FALSE,
                append = TRUE)
  }

  x <- calcOutput("IFuelPrice", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  # write input data file that GAMS can read
  xq <- as.quitte(x)
  xq <- xq[!is.na(xq[["value"]]), ] %>%
        select(c("period", "value", "region", "variable", "new")) %>% # nolint
        pivot_wider(names_from = "period") # nolint
  fheader <- paste("dummy,dummy,dummy", paste(colnames(xq)[4 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iFuelPrice.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iFuelPrice.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput("INewReg", aggregate = TRUE)
  # write input data file that GAMS can read
  xq <- as.quitte(x) %>%
        select(c("period", "value", "region")) %>% # nolint
        pivot_wider(names_from = "period") # nolint
  fheader <- paste("dummy", paste(colnames(xq)[2 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iNewReg.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iNewReg.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput("ITransChar", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  # write input data file that GAMS can read
  xq <- as.quitte(x)
  xq <- xq[!is.na(xq[["value"]]), ] %>%
        select(c("period", "value", "region", "variable")) %>% # nolint
        pivot_wider(names_from = "period") # nolint
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iTransChar.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iTransChar.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

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
              append = TRUE)

  x <- calcOutput("IDataElecSteamGen", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("period", "value", "region", "variable")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataElecSteamGen.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iDataElecSteamGen.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput("IDataDistrLosses", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataDistrLosses.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iDataDistrLosses.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput("IDataConsEneBranch", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataConsEneBranch.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iDataConsEneBranch.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput("IDataImports", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataImports.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iDataImports.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput("ISuppExports", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iSuppExports.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iSuppExports.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput("IDataTransTech", aggregate = FALSE)
  x <- as.quitte(x)
  x <- select(x, c("transfinal", "ttech", "value", "variable", "period")) %>%
    pivot_wider(names_from = "period")
  xq <- x
  fheader <- paste("dummy,dummy,dummy", paste(colnames(xq)[4 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataTransTech.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iDataTransTech.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput("IEnvPolicies", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  xq <- as.quitte(x) %>%
    select(c("region", "policies_set", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iEnvPolicies.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iEnvPolicies.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "IDataElecProd", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataElecProd.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iDataElecProd.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "IMaxResPot", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iMaxResPot.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iMaxResPot.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "IMinResPot", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iMinResPot.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iMinResPot.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "IVarCost", aggregate = FALSE)
  xq <- as.quitte(x) %>%
    select(c("variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy", paste(colnames(xq)[2 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iVarCost.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iVarCost.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "IDataPlantEffByType", aggregate = FALSE)
  xq <- as.quitte(x) %>%
    select(c("variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy", paste(colnames(xq)[2 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataPlantEffByType.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iDataPlantEffByType.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

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
              append = TRUE)

  x <- calcOutput(type = "IAvailRate", aggregate = FALSE)
  xq <- as.quitte(x) %>%
    select(c("variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy", paste(colnames(xq)[2 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iAvailRate.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iAvailRate.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "IFixOandMCost", aggregate = FALSE)
  xq <- as.quitte(x) %>%
    select(c("variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy", paste(colnames(xq)[2 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iFixOandMCost.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iFixOandMCost.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "IRateLossesFinCons", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  # write input data file that GAMS can read
  xq <- as.quitte(x)
  xq <- xq[!is.na(xq[["value"]]), ] %>%
        select(c("period", "value", "region", "variable")) %>% # nolint
        pivot_wider(names_from = "period") # nolint
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iRateLossesFinCons.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iRateLossesFinCons.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "IGrossCapCosSubRen", aggregate = FALSE)
  xq <- as.quitte(x) %>%
    select(c("variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy", paste(colnames(xq)[2 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "IGrossCapCosSubRen.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "IGrossCapCosSubRen.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput("IInstCapPast", aggregate = TRUE)
  variable <- NULL
  xq <- as.quitte(x) %>% filter(variable != "PGNUC") %>%
    select(c("period", "value", "region", "variable")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iInstCapPast.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iInstCapPast.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "ISuppPrimprod", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iSuppPrimProd.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iSuppPrimProd.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "ISuppRatePrimProd", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iSuppRatePrimProd.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iSuppRatePrimProd.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "ISuppRefCapacity", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iSuppRefCapacity.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iSuppRefCapacity.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)
  
  x <- calcOutput(type = "IElcNetImpShare", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  # write input data file that GAMS can read
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iElcNetImpShare.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iElcNetImpShare.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "IDataGrossInlCons", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataGrossInlCons.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iDataGrossInlCons.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "IMatFacPlaAvailCap", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iMatFacPlaAvailCap.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iMatFacPlaAvailCap.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "IInvPlants", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iInvPlants.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iInvPlants.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "IDecomPlants", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDecomPlants.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iDecomPlants.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "ISupRateEneBranCons", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iSupRateEneBranCons.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iSupRateEneBranCons.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "IPriceFuelsInt", aggregate = FALSE)
  xq <- as.quitte(x) %>%
    select(c("variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy", paste(colnames(xq)[2 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iPriceFuelsInt.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iPriceFuelsInt.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "INatGasPriProElst", aggregate = FALSE)
  # POP is weights for aggregation, perform aggregation
  x <- toolAggregate(x, weight = POP, rel = map, from = "ISO3.Code", to = "Region.Code")
  xq <- as.quitte(x) %>% select(c("region", "value"))
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iNatGasPriProElst.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "IPriceFuelsIntBase", aggregate = FALSE)
  xq <- as.quitte(x) %>%
    select(c("variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy", paste(colnames(xq)[2 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iPriceFuelsIntBase.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iPriceFuelsIntBase.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "IDataTransfOutputRef", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataTransfOutputRef.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iDataTransfOutputRef.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)
  
  x <- calcOutput(type = "IDataTotTransfInputRef", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataTotTransfInputRef.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iDataTotTransfInputRef.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "ISuppTransfers", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("region", "variable", "period", "value")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iSuppTransfers.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iSuppTransfers.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)


  return(list(x = x,
              weight = NULL,
              unit = "various",
              description = "OPENPROM input data"))

}
