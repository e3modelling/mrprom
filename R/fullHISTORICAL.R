#' fullHISTORICAL
#'
#' Read data, convert it to a mrprom mif file so to compare output mif file
#' with OPEN-PROM output.
#'
#' @return The mif file
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- retrieveData("HISTORICAL", regionmapping = "regionmappingOP.csv")
#' }
#'
#' @importFrom quitte as.quitte write.mif
#' @importFrom dplyr select
#' @export

fullHISTORICAL <- function() {

  y <- NULL
  new <- NULL
  data <- NULL
  for (i in c("NENSE", "DOMSE", "INDSE", "TRANSE")) {
    x <- calcOutput(type = "IFuelCons", subtype = i, aggregate = TRUE)
    x[is.na(x)] <- 0
    xq <- as.quitte(x)
    xq["variable"] <- paste(i, xq[["variable"]], xq[["new"]])
    xq <- select((xq), -c(new))
    xq["model"] <- "Enerdata fuel consumption"
    y <- rbind(y, xq)
  }

  gdp <- calcOutput("iGDP", aggregate = TRUE)
  q1 <- as.quitte(gdp)
  q1["variable"] <- "GDP|PPP"
  q1["unit"] <- "billion US$2015/yr"
  q1["model"] <- "SSP"

  POP <- calcOutput("POP", aggregate = TRUE)
  q2 <- as.quitte(POP)
  q2["variable"] <- "Population"
  q2["unit"] <- "billion"
  q2["model"] <- "SSP"
  q <- rbind(q1, q2)
  q <- select((q), -c(data))

  ACTV <- calcOutput(type = "ACTV", aggregate = TRUE)
  q3 <- as.quitte(ACTV)
  q3["unit"] <- "various"
  q3["model"] <- "GEME3, IRF"
  q3["variable"] <- paste(deparse(substitute(ACTV)), q3[["variable"]])

  IDataElecSteamGen <- calcOutput(type = "IDataElecSteamGen", aggregate = TRUE)
  q4 <- as.quitte(IDataElecSteamGen)
  q4["unit"] <- "GW"
  q4["model"] <- "Enerdata Installed capacity"
  q4["variable"] <- paste(deparse(substitute(IDataElecSteamGen)), q4[["variable"]])

  IDataPassCars <- calcOutput(type = "IDataPassCars", aggregate = FALSE)
  q5 <- as.quitte(IDataPassCars)
  q5["unit"] <- "reuse_pc"
  q5["model"] <- "IRF"
  q5["variable"] <- "PC scr"

  INewReg <- calcOutput(type = "INewReg", aggregate = TRUE)
  q6 <- as.quitte(INewReg)
  q6["unit"] <- "million vehicles"
  q6["model"] <- "IRF"
  q6["variable"] <- paste(deparse(substitute(INewReg)), q6[["variable"]])

  ITransChar <- calcOutput(type = "ITransChar", aggregate = FALSE)
  q7 <- as.quitte(ITransChar)
  q7["unit"] <- "Thousands km/veh"
  q7["model"] <- "IRF"
  q7["variable"] <- paste(deparse(substitute(ITransChar)), q7[["variable"]])

  IFuelPrice <- calcOutput(type = "IFuelPrice", aggregate = FALSE)
  q8 <- as.quitte(IFuelPrice)
  q8["unit"] <- "various"
  q8["model"] <- "Enerdata fuel price in all sectors"
  q8["variable"] <- paste(deparse(substitute(IFuelPrice)), q8[["variable"]], q8[["new"]])
  q8 <- select(q8, -c(new))
  
  IDataElecAndSteamGen <- calcOutput("IDataElecAndSteamGen", aggregate = TRUE)
  q9 <- as.quitte(IDataElecAndSteamGen)
  q9["unit"] <- "GW"
  q9["model"] <- "Historical CHP Capacity"
  q9["variable"] <- paste(deparse(substitute(IDataElecAndSteamGen)), q9[["variable"]])
  
  IDataDistrLosses <- calcOutput("IDataDistrLosses", aggregate = TRUE)
  q10 <- as.quitte(IDataDistrLosses)
  q10["unit"] <- "Mtoe"
  q10["model"] <- "Enerdata; Distribution Losses"
  q10["variable"] <- paste(deparse(substitute(IDataDistrLosses)), q10[["variable"]])
  
  IDataConsEneBranch <- calcOutput("IDataConsEneBranch", aggregate = TRUE)
  q11 <- as.quitte(IDataConsEneBranch)
  q11["unit"] <- "Mtoe"
  q11["model"] <- "Enerdata; Consumption of Energy Branch"
  q11["variable"] <- paste(deparse(substitute(IDataConsEneBranch)), q11[["variable"]])
  
  IDataImports <- calcOutput("IDataImports", aggregate = TRUE)
  q12 <- as.quitte(IDataImports)
  q12["unit"] <- "Mtoe"
  q12["model"] <- "Enerdata; Fuel Imports"
  q12["variable"] <- paste(deparse(substitute(IDataImports)), q12[["variable"]])
  
  ISuppExports <- calcOutput("ISuppExports", aggregate = TRUE)
  q13 <- as.quitte(ISuppExports)
  q13["unit"] <- "Mtoe"
  q13["model"] <- "Enerdata; Fuel Exports"
  q13["variable"] <- paste(deparse(substitute(ISuppExports)), q13[["variable"]])
  
  ttech <- NULL
  transfinal <- NULL
  IDataTransTech <- calcOutput("IDataTransTech", aggregate = FALSE)
  q14 <- as.quitte(IDataTransTech)
  q14["unit"] <- "various"
  q14["model"] <- "readTechCosts;EU Reference Scenario and MENA_EDS"
  q14["variable"] <- paste(deparse(substitute(IDataTransTech)), q14[["variable"]], q14[["ttech"]], q14[["transfinal"]])
  q14 <- select(q14, -c(ttech, transfinal))
  
  policies_set <- NULL
  IEnvPolicies <- calcOutput("IEnvPolicies", aggregate = TRUE)
  q15 <- as.quitte(IEnvPolicies)
  q15["unit"] <- "various"
  q15["model"] <- "Carbon price data"
  q15["variable"] <- paste(deparse(substitute(IEnvPolicies)), q15[["variable"]], q15[["policies_set"]])
  q15 <- select(q15, -c(policies_set))
  
  z <- rbind(y, q, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14, q15)
  write.mif(z, "mrprom.mif", append = FALSE)
}
