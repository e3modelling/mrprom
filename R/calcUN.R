#' calcUN
#'
#' Use UN data for fuel consumption in DOMSE, INDSE, NENSE, TRANSE
#' 
#' @return UN fuel consumption in DOMSE, INDSE, NENSE, TRANSE
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "UN", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select full_join
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom utils tail


calcUN <- function() {
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  #UN data
  x <- readSource("UN")
  
  x <- x[,,getItems(x,3.3)[!(getItems(x,3.3) %in% c("FE","REN"))]]
  
  x <- x[,,c("CH","NF","BM","FD","PP","TX","EN","OI","SE","AG","HOU","IS","NEN","PT","PN","PC","PA")]
  
  a6 <- readSource("IRF", subtype = "inland-surface-passenger-transport-by-rail")
  #million pKm/yr
  a7 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-rail")
  #million tKm/yr
  a6 <- a6[, Reduce(intersect, list(getYears(a6), getYears(a7), getYears(x))), ]
  a7 <- a7[, Reduce(intersect, list(getYears(a6), getYears(a7), getYears(x))), ]
  x <- x[, Reduce(intersect, list(getYears(a6), getYears(a7), getYears(x))), ]
  
  out3 <- (a6 / (a6 + a7))
  out4 <- (a7 / (a6 + a7))
  
  GT <- x[, , "PT"]
  
  getItems(GT, 3.1) <- "GT"
  
  out4 <- collapseDim(out4, 3)
  
  #inland-surface-freight-transport-by-rail / total inland-surface
  GT <- GT * ifelse(is.na(out4), mean(out4, na.rm=TRUE), out4)
  
  x <- mbind(x, GT)
  
  #inland-surface-passenger-transport-by-rail / total inland-surface transport-by-rail
  x[, , "PT"] <- x[, , "PT"] * ifelse(is.na(out3), mean(out3, na.rm=TRUE), out3)
  

  a8 <- readSource("IRF", subtype = "passenger-car-traffic")
  #million motor vehicles Km/yr
  a9 <- readSource("IRF", subtype = "total-four-wheeled-traffic")
  #million motor vehicles Km/yr
  a8 <- a8[, Reduce(intersect, list(getYears(a8), getYears(a9), getYears(x))), ]
  a9 <- a9[, Reduce(intersect, list(getYears(a8), getYears(a9), getYears(x))), ]
  x <- x[, Reduce(intersect, list(getYears(a8), getYears(a9), getYears(x))), ]
  
  out1 <- (a8 / a9)
  
  a10 <- readSource("IRF", subtype = "total-van,-pickup,-lorry-and-road-tractor-traffic")
  #million motor vehicles Km/yr
  a11 <- readSource("IRF", subtype = "total-four-wheeled-traffic")
  #million motor vehicles Km/yr
  a10 <- a10[, Reduce(intersect, list(getYears(a10), getYears(a11), getYears(x))), ]
  a11 <- a11[, Reduce(intersect, list(getYears(a10), getYears(a11), getYears(x))), ]
  x <- x[, Reduce(intersect, list(getYears(a10), getYears(a11), getYears(x))), ]
  
  out2 <- (a10 / a11)
  
  GU <- x[, , "PC"]
  
  getItems(GU, 3.1) <- "GU"
  
  out2 <- collapseDim(out2, 3)
  
  #inland-surface-freight-transport-by-rail / total inland-surface
  GU <- GU * ifelse(is.na(out2), mean(out2, na.rm=TRUE), out2)
  
  x <- mbind(x, GU)
  
  #PB
  a12 <- readSource("IRF", subtype = "bus-and-motor-coach-traffic")
  #million motor vehicles Km/yr
  a13 <- readSource("IRF", subtype = "total-four-wheeled-traffic")
  #million motor vehicles Km/yr
  a12 <- a12[, Reduce(intersect, list(getYears(a12), getYears(a13), getYears(x))), ]
  a13 <- a13[, Reduce(intersect, list(getYears(a12), getYears(a13), getYears(x))), ]
  x <- x[, Reduce(intersect, list(getYears(a12), getYears(a13), getYears(x))), ]
  
  out5 <- (a12 / a13)
  
  out5 <- collapseDim(out5, 3)
  
  PB <- x[, , "PC"]
  
  getItems(PB, 3.1) <- "PB"
  
  out2 <- collapseDim(out2, 3)
  
  #bus-and-motor-coach-traffic / total-van,-pickup,-lorry-and-road-tractor-traffic
  PB <- PB * ifelse(is.na(out5), mean(out5, na.rm=TRUE), out5)
  
  x <- mbind(x, PB)
  
  #passenger-car-traffic / total-van,-pickup,-lorry-and-road-tractor-traffic
  x[, , "PC"] <- x[, , "PC"] * ifelse(is.na(out1), mean(out1, na.rm=TRUE), out1)
  
  #PN and GN
  a14 <- readSource("TREMOVE", subtype = "Stock")
  a14 <- a14[,,"REF"][,,"NAVIGATION"]
  PN <- dimSums(a14[,,"Passenger"],3)
  GN <- dimSums(a14[,,"Freight"],3)
  PN <- as.quitte(PN) %>%
    interpolate_missing_periods(period = getYears(PN, as.integer = TRUE)[1] : last(getYears(PN, as.integer = TRUE)), expand.values = TRUE)
  PN <- as.magpie(PN)
  GN <- as.quitte(GN) %>%
    interpolate_missing_periods(period = getYears(GN, as.integer = TRUE)[1] : last(getYears(GN, as.integer = TRUE)), expand.values = TRUE)
  GN <- as.magpie(GN)
  
  PN <- PN[, Reduce(intersect, list(getYears(PN), getYears(GN),getYears(x))), ]
  GN <- GN[, Reduce(intersect, list(getYears(PN), getYears(GN),getYears(x))), ]
  x <- x[, Reduce(intersect, list(getYears(PN), getYears(GN),getYears(x))), ]
  
  #million pKm/yr
  In_Nav <- PN + GN
  
  out6 <- (PN / In_Nav)
  out7 <- (GN / In_Nav)
  out7 <- toolCountryFill(out7, fill = NA)
  out6 <- toolCountryFill(out6, fill = NA)
  
  GN <- x[, , "PN"]
  
  getItems(GN, 3.1) <- "GN"
  
  out7 <- collapseDim(out7, 3)
  
  #Passenger inland navigation / inland navigation
  GN <- GN * ifelse(is.na(out7), mean(out7, na.rm=TRUE), out7)
  
  x <- mbind(x, GN)
  
  #Freight inland navigation / inland navigation
  x[, , "PN"] <- x[, , "PN"] * ifelse(is.na(out6), mean(out6, na.rm=TRUE), out6)
  
  # complete incomplete time series
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
  
  # qx[["new"]] <- as.character(qx[["new"]])
  # qx[["new"]][qx[["new"]] == "STE"] <- "STE1AH"
  
  x <- as.quitte(qx) %>% as.magpie()
  # set NA to 0
  x[is.na(x)] <- 10^-6
  x <- x[,fStartHorizon : 2100,]
  
  list(x = x,
       weight = NULL,
       unit = "Mtoe",
       description = "UN fuel consumption in DOMSE, INDSE, NENSE, TRANSE")
  
}
