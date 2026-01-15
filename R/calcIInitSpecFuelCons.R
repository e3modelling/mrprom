#' calcIInitSpecFuelCons
#'
#' Use ENERDATA and IRF data to derive OPENPROM input parameter
#' iInitSpecFuelCons.
#'
#' @return  OPENPROM input data iInitSpecFuelCons.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IInitSpecFuelCons", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>% left_join mutate select last

calcIInitSpecFuelCons <- function() {

  x <- calcOutput(type = "IFuelCons2", subtype = "TRANSE", aggregate = FALSE)
  #Mtoe
  x <- x * 1000
  #ktoe
  x <- x[, "y2017", ]

  x2 <- readSource("IRF", subtype = "passenger-car-traffic")
  #million motor vehicles Km/yr
  x2 <- x2 * (1.7)
  #motor vehicle Gpkm/yr

  x <- x[, Reduce(intersect, list(getYears(x), getYears(x2)))]
  x2 <- x2[, Reduce(intersect, list(getYears(x), getYears(x2)))]
  
  #consumption data / passenger-car-traffic
  PC <- (x / x2) * 100

  PC <- PC[, , "PC"]
  
  PB_tr <- readSource("IRF", subtype = "bus-and-motor-coach-traffic")
  #Km/yr
  PB_tr <- PB_tr * (50) / 1000 #50 passengers
  #million pKm/yr
  
  x <- x[, Reduce(intersect, list(getYears(x), getYears(PB_tr)))]
  PB_tr <- PB_tr[, Reduce(intersect, list(getYears(x), getYears(PB_tr)))]
  
  #consumption data / passenger-car-traffic
  PB <- (x / PB_tr) * 100
  
  PB <- PB[, , "PB"]

  x3 <- readSource("IRF", subtype = "inland-surface-passenger-transport-by-rail")
  #million pKm/yr

  x <- x[, Reduce(intersect, list(getYears(x), getYears(x3)))]
  x3 <- x3[, Reduce(intersect, list(getYears(x), getYears(x3)))]
  
  #consumption data / inland-surface-passenger-transport-by-rail
  PT <- (x / x3) * 100

  PT <- PT[, , "PT"]

  #The data has information about transport passengers per country and per year for aviation
  x4 <- readSource("Eurostat_Transport")
  
  #pkm/yr
  x4 <- x4 / 10^6
  #million pKm/yr

  x <- x[, Reduce(intersect, list(getYears(x), getYears(x4)))]
  x4 <- x4[, Reduce(intersect, list(getYears(x), getYears(x4)))]

  #consumption data / passengers per country and per year for aviation
  PA <- (x / x4) * 100

  PA <- PA[, , "PA"]

  x6 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-road")
  #million tKm/yr

  x <- x[, Reduce(intersect, list(getYears(x), getYears(x6)))]
  x6 <- x6[, Reduce(intersect, list(getYears(x), getYears(x6)))]
  
  #consumption data / inland-surface-freight-transport-by-road
  GU <- (x / x6) * 100

  GU <- GU[, , "GU"]

  x7 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-rail")
  #million tKm/yr

  x <- x[, Reduce(intersect, list(getYears(x), getYears(x7)))]
  x7 <- x7[, Reduce(intersect, list(getYears(x), getYears(x7)))]

  GT <- (x / x7) * 100

  GT <- GT[, , "GT"]

  x8 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-inland-waterway")
  #million tKm/yr

  x <- x[, Reduce(intersect, list(getYears(x), getYears(x8)))]
  x8 <- x8[, Reduce(intersect, list(getYears(x), getYears(x8)))]
  
  #consumption data / inland-surface-freight-transport-by-inland-waterway
  GN <- (x / x8) * 100

  GN <- GN[, , "GN"]
  
  pn <- readSource("TREMOVE", subtype = "Stock")
  pn <- pn[,,"REF"][,,"NAVIGATION"][,,"Passenger"]
  pn <- dimSums(pn[,,"Passenger"],3)
  suppressMessages(
    suppressWarnings(
      pn <- toolCountryFill(pn, fill = NA)
    )
  )

  pn <- as.quitte(pn) %>%
    interpolate_missing_periods(period = getYears(pn, as.integer = TRUE)[1] : last(getYears(pn, as.integer = TRUE)), expand.values = TRUE)
  pn <- pn %>% filter(`period` %in% getYears(x, as.integer = TRUE))
  pn[["value"]] <- pn[["value"]] / 10^3
  pn[["unit"]] <- "million pKm/yr"
  pn[["variable"]] <- "inland-surface-passenger-transport-by-inland-waterway"
  pn <- as.quitte(pn)
  pn <- as.magpie(pn)
  
  x <- x[, Reduce(intersect, list(getYears(x), getYears(pn)))]
  pn <- pn[, Reduce(intersect, list(getYears(x), getYears(pn)))]
  
  #consumption data / inland-surface-passenger-transport-by-rail
  pn <- collapseDim(pn,3)
  PN <- (x[, , "PN"] / pn) * 100
  
  PN <- PN[, , "PN"]

  PA <- as.quitte(PA)
  PC <- as.quitte(PC)
  PB <- as.quitte(PB)
  PT <- as.quitte(PT)
  GT <- as.quitte(GT)
  GU <- as.quitte(GU)
  GN <- as.quitte(GN)
  PN <- as.quitte(PN)
  #ktoe/Gpkm

  unit1 <- NULL
  unit <- NULL
  variable1 <- NULL

  PA <- select((PA), -c(unit1, unit))
  PC <- select((PC), -c(variable1, unit1, unit))
  PT <- select((PT), -c(variable1, unit1, unit))
  PB <- select((PB), -c(variable1, unit1, unit))
  GN <- select((GN), -c(variable1, unit1, unit))
  GU <- select((GU), -c(variable1, unit1, unit))
  GT <- select((GT), -c(variable1, unit1, unit))
  PN <- select((PN), -c(variable1, unit1, unit))
  
  y <- rbind(PC, PT, PA, GU, GT, GN, PB, PN)

  qx <- as.quitte(y) %>% as.magpie()

  qx <- as.quitte(qx)
  qx_bu <- qx
  # assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  names(qx) <- sub("region", "CountryCode", names(qx))
  ## add h12 mapping to dataset
  qx <- left_join(qx, h12, by = "CountryCode")
  ## add new column containing regional mean value
  value <- NULL
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "new", "variable"))
  names(qx) <- sub("CountryCode", "region", names(qx))
  qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  qx_bu <- select(qx_bu, -c("model", "scenario"))
  ## assign to countries with NA, their H12 region mean
  value.x <- NULL
  value.y <- NULL
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "new", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  ## assign to countries that still have NA, the global mean
  qx_bu <- qx
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "new", "variable"))
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "new", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  x <- as.quitte(qx) %>% as.magpie()
  # set NA to 0
  x[is.na(x)] <- 0

  list(x = x,
       weight = NULL,
       unit = "ktoe/Gpkm",
       description = "iInitSpecFuelCons")

}
