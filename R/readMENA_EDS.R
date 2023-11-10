#' readMENA_EDS
#'
#' Read MENA_EDS gdx files, convert it to a MENA_EDS mif file so to compare output mif file
#' with OPEN-PROM output.
#'
#' @return The read-in data into a mif object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("MENA_EDS")
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom dplyr select
#' @importFrom quitte as.quitte write.mif

readMENA_EDS <- function() {

  TRANSE <- NULL
  EF <- NULL
  data <- NULL
  #TRANSE
  DEMTR <- readGDX(gdx = "fulldata.gdx", name = c("DEMTR"), field = "l")
  DEMTR <- as.quitte(DEMTR)
  DEMTR["model"] <- "MENA_EDS"
  DEMTR["variable"] <- paste(DEMTR$TRANSE, DEMTR$EF)
  DEMTR <- select((DEMTR), -c(TRANSE, EF))
  DEMTR["unit"] <- "Mtoe"

  NENSE <- NULL
  #NENSE
  Nen_FCon <- readGDX(gdx = "fulldata.gdx", name = c("Nen_FCon"), field = "l")
  Nen_FCon <- as.quitte(Nen_FCon)
  Nen_FCon["model"] <- "MENA_EDS"
  Nen_FCon["variable"] <- paste(Nen_FCon$NENSE, Nen_FCon$EF)
  Nen_FCon <- select((Nen_FCon), -c(NENSE, EF))
  Nen_FCon["unit"] <- "Mtoe"

  DOMSE <- NULL
  #DOMSE
  Dom_FCon <- readGDX(gdx = "fulldata.gdx", name = c("Dom_FCon"), field = "l")
  Dom_FCon <- as.quitte(Dom_FCon)
  Dom_FCon["model"] <- "MENA_EDS"
  Dom_FCon["variable"] <- paste(Dom_FCon$DOMSE, Dom_FCon$EF)
  Dom_FCon <- select((Dom_FCon), -c(DOMSE, EF))
  Dom_FCon["unit"] <- "Mtoe"

  INDSE <- NULL
  #INDSE
  Indu_FCon <- readGDX(gdx = "fulldata.gdx", name = c("Indu_FCon"), field = "l")
  Indu_FCon <- as.quitte(Indu_FCon)
  Indu_FCon["model"] <- "MENA_EDS"
  Indu_FCon["variable"] <- paste(Indu_FCon$INDSE, Indu_FCon$EF)
  Indu_FCon <- select((Indu_FCon), -c(INDSE, EF))
  Indu_FCon["unit"] <- "Mtoe"

  PGOTH <- NULL
  #iDataElecSteamGen
  iDataElecSteamGen <- readGDX(gdx = "fulldata.gdx", name = c("PG_Oth"), field = "l")
  iDataElecSteamGen <- as.quitte(iDataElecSteamGen)
  iDataElecSteamGen["model"] <- "MENA_EDS"
  iDataElecSteamGen["variable"] <- iDataElecSteamGen["PGOTH"]
  iDataElecSteamGen <- select((iDataElecSteamGen), -c(PGOTH))
  iDataElecSteamGen["unit"] <- "varius"

  Gompset1 <- NULL
  Gompset2 <- NULL
  #iDataPassCars
  iDataPassCars <- readGDX(gdx = "fulldata.gdx", name = c("Trans_Gomp"), field = "l")
  iDataPassCars <- as.quitte(iDataPassCars)
  iDataPassCars["model"] <- "MENA_EDS"
  iDataPassCars["variable"] <- paste(iDataPassCars$Gompset1, iDataPassCars$Gompset2)
  iDataPassCars <- select((iDataPassCars), -c(Gompset1, Gompset2))
  #iDataPassCars <- iDataPassCars[nrow(iDataPassCars), ]
  iDataPassCars["unit"] <- "reuse_pc"

  SBS <- NULL
  #iFuelPrice
  iFuelPrice <- readGDX(gdx = "fulldata.gdx", name = c("DompricePRN"), field = "l")
  iFuelPrice <- as.quitte(iFuelPrice)
  iFuelPrice["model"] <- "MENA_EDS"
  iFuelPrice["variable"] <- paste(iFuelPrice$SBS, iFuelPrice$EF)
  iFuelPrice <- select((iFuelPrice), -c(SBS, EF))
  iFuelPrice["unit"] <- "$2015/toe"

  #iNewReg
  NewReg <- readGDX(gdx = "fulldata.gdx", name = c("NewReg"), field = "l")
  NewReg <- as.quitte(NewReg)
  NewReg["model"] <- "MENA_EDS"
  NewReg["variable"] <- "passenger-car-first-registrations"
  NewReg <- select((NewReg), -c(data))
  NewReg["unit"] <- "million vehicles"

  TRANSPCHAR <- NULL
  #iTransChar
  iTransChar <- readGDX(gdx = "fulldata.gdx", name = c("TRANSCHAR"), field = "l")
  iTransChar <- as.quitte(iTransChar)
  iTransChar["model"] <- "MENA_EDS"
  iTransChar["variable"] <- iTransChar["TRANSPCHAR"]
  iTransChar <- select((iTransChar), -c(TRANSPCHAR))
  iTransChar["unit"] <- "Thousands km/veh"
  
  SBS <- NULL
  #ACTV
  ACTV <- readGDX(gdx = "fulldata.gdx", name = c("ACTV"), field = "l")
  ACTV <- as.quitte(ACTV)
  ACTV["model"] <- "MENA_EDS"
  ACTV["variable"] <- ACTV["SBS"]
  ACTV <- select((ACTV), -c(SBS))
  ACTV["unit"] <- "various"

  iGDP <- readGDX(gdx = "fulldata.gdx", name = c("GDP"), field = "l")
  q1 <- as.quitte(iGDP)

  q1["variable"] <- "GDP|PPP"
  q1["unit"] <- "billion US$2015/yr"
  q1["model"] <- "MENA_EDS"

  POP <- readGDX(gdx = "fulldata.gdx", name = c("POP"), field = "l")
  q2 <- as.quitte(POP)
  q2["variable"] <- "Population"
  q2["unit"] <- "billion"
  q2["model"] <- "MENA_EDS"

  q <- rbind(q1, q2)
  q <- select((q), -c(data))

  z <- rbind(DEMTR, Nen_FCon, Dom_FCon, Indu_FCon, iDataElecSteamGen,
             iDataPassCars,iFuelPrice, NewReg, iTransChar, ACTV, q)
  write.mif(z, "MENA_EDS.mif", append = FALSE)

  return(suppressWarnings(as.magpie(z)))
}
