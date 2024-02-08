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
    xq["unit"] <- "various"
    xq["model"] <- "Enerdata"
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
  q3["model"] <- "GEME3 and IRF"
  q3["variable"] <- paste("ACTV", q3[["variable"]])

  IDataElecSteamGen <- calcOutput(type = "IDataElecSteamGen", aggregate = TRUE)
  q4 <- as.quitte(IDataElecSteamGen)
  q4["unit"] <- "GW"
  q4["model"] <- "Enerdata"
  q4["variable"] <- paste("IDataElecSteamGen", q4[["variable"]])

  IDataPassCars <- calcOutput(type = "IDataPassCars", aggregate = FALSE)
  q5 <- as.quitte(IDataPassCars)
  q5["unit"] <- "%"
  q5["model"] <- "IRF"
  q5["variable"] <- "IDataPassCars PC_scr_rate"

  INewReg <- calcOutput(type = "INewReg", aggregate = TRUE)
  q6 <- as.quitte(INewReg)
  q6["unit"] <- "million vehicles"
  q6["model"] <- "IRF"
  q6["variable"] <- paste("INewReg", q6[["variable"]])

  ITransChar <- calcOutput(type = "ITransChar", aggregate = FALSE)
  q7 <- as.quitte(ITransChar)
  q7["unit"] <- "Thousands km/veh"
  q7["model"] <- "IRF"
  q7["variable"] <- paste("ITransChar", q7[["variable"]])

  IFuelPrice <- calcOutput(type = "IFuelPrice", aggregate = FALSE)
  q8 <- as.quitte(IFuelPrice)
  q8["unit"] <- "various"
  q8["model"] <- "Enerdata"
  q8["variable"] <- paste("IFuelPrice", q8[["variable"]], q8[["new"]])
  q8 <- select(q8, -c(new))

  IDataElecAndSteamGen <- calcOutput("IDataElecAndSteamGen", aggregate = TRUE)
  q9 <- as.quitte(IDataElecAndSteamGen)
  q9["unit"] <- "GW"
  q9["model"] <- "Historical CHP Capacity"
  q9["variable"] <- paste("IDataElecAndSteamGen", q9[["variable"]])

  IDataDistrLosses <- calcOutput("IDataDistrLosses", aggregate = TRUE)
  q10 <- as.quitte(IDataDistrLosses)
  q10["unit"] <- "Mtoe"
  q10["model"] <- "Enerdata"
  q10["variable"] <- paste("IDataDistrLosses", q10[["variable"]])

  IDataConsEneBranch <- calcOutput("IDataConsEneBranch", aggregate = TRUE)
  q11 <- as.quitte(IDataConsEneBranch)
  q11["unit"] <- "Mtoe"
  q11["model"] <- "Enerdata"
  q11["variable"] <- paste("IDataConsEneBranch", q11[["variable"]])

  IDataImports <- calcOutput("IDataImports", aggregate = TRUE)
  q12 <- as.quitte(IDataImports)
  q12["unit"] <- "Mtoe"
  q12["model"] <- "Enerdata"
  q12["variable"] <- paste("IDataImports", q12[["variable"]])

  ISuppExports <- calcOutput("ISuppExports", aggregate = TRUE)
  q13 <- as.quitte(ISuppExports)
  q13["unit"] <- "Mtoe"
  q13["model"] <- "Enerdata"
  q13["variable"] <- paste("ISuppExports", q13[["variable"]])

  ttech <- NULL
  transfinal <- NULL
  IDataTransTech <- calcOutput("IDataTransTech", aggregate = FALSE)
  q14 <- as.quitte(IDataTransTech)
  q14["unit"] <- "various"
  q14["model"] <- "EU Reference Scenario and MENA_EDS"
  q14["variable"] <- paste("IDataTransTech", q14[["variable"]], q14[["ttech"]], q14[["transfinal"]])
  q14 <- select(q14, -c(ttech, transfinal))

  policies_set <- NULL
  IEnvPolicies <- calcOutput("IEnvPolicies", aggregate = TRUE)
  q15 <- as.quitte(IEnvPolicies)
  q15["unit"] <- "various"
  q15["model"] <- " EU Reference Scenario 2020 and ENGAGE project"
  q15["variable"] <- paste("IEnvPolicies", q15[["policies_set"]])
  q15 <- select(q15, -c(policies_set))

  z <- rbind(y, q, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14, q15)
  z <- as.quitte(z)
  write.mif(z, "mrprom.mif", append = FALSE)

  return(list(x = x,
              weight = NULL,
              unit = "various",
              description = "HISTORICAL"))

}
