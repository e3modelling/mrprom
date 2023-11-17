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
  variable <- NULL
  new <- NULL
  data <- NULL
  for (i in c("NENSE", "DOMSE", "INDSE", "TRANSE")) {
    x <- calcOutput(type = "IFuelCons", subtype = i, aggregate = TRUE)
    x[is.na(x)] <- 0
    xq <- as.quitte(x)
    xq$variable <- paste(xq$variable, xq$new)
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

  IDataElecSteamGen <- calcOutput(type = "IDataElecSteamGen", aggregate = TRUE)
  q4 <- as.quitte(IDataElecSteamGen)
  q4["unit"] <- "GW"
  q4["model"] <- "Enerdata Installed capacity"

  IDataPassCars <- calcOutput(type = "IDataPassCars", aggregate = FALSE)
  q5 <- as.quitte(IDataPassCars)
  q5["unit"] <- "reuse_pc"
  q5["model"] <- "IRF"
  q5["variable"] <- "PC scr"

  INewReg <- calcOutput(type = "INewReg", aggregate = TRUE)
  q6 <- as.quitte(INewReg)
  q6["unit"] <- "million vehicles"
  q6["model"] <- "IRF"

  ITransChar <- calcOutput(type = "ITransChar", aggregate = FALSE)
  q7 <- as.quitte(ITransChar)
  q7["unit"] <- "Thousands km/veh"
  q7["model"] <- "IRF"

  IFuelPrice <- calcOutput(type = "IFuelPrice", aggregate = FALSE)
  q8 <- as.quitte(IFuelPrice)
  q8["unit"] <- "various"
  q8["model"] <- "Enerdata fuel price in all sectors"
  q8["variable"] <- paste(q8$variable, q8$new)
  q8 <- select(q8, -c(new))

  z <- rbind(y, q, q3, q4, q5, q6, q7, q8)
  write.mif(z, "mrprom.mif", append = FALSE)
}
