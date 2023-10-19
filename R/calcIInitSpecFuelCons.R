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
#' @importFrom dplyr %>% left_join mutate select filter

calcIInitSpecFuelCons <- function() {
  
  x <- calcOutput(type = "IFuelCons", subtype = "TRANSE", aggregate = FALSE)
  #Mtoe
  x <- x*1000
  #ktoe
  x <- x[,"y2017",]
  
  x2 <- readSource("IRF", subtype = "passenger-car-traffic")
  #million motor vehicles Km/yr
  x2 <- x2*(1.7)/1000
  #motor vehicle Bpkm/yr
  
  x <- x[,Reduce(intersect, list(getYears(x),getYears(x2)))]
  x2 <- x2[,Reduce(intersect, list(getYears(x),getYears(x2)))]
  
  x2[x2 < 0.1] <- NA
  
  PC <- (x/x2)
  
  PC <- PC[,,"PC"]
  
  x3 <- readSource("IRF", subtype = "inland-surface-passenger-transport-by-rail")
  #million pKm/yr
  x3 <- x3/1000
  #BpKm/yr
  
  x <- x[,Reduce(intersect, list(getYears(x),getYears(x3)))]
  x3 <- x3[,Reduce(intersect, list(getYears(x),getYears(x3)))]
  
  x3[x3 < 0.1] <- NA
  
  PT <- (x/x3)
  
  PT <- PT[,,"PT"]
  
  x4 <- readSource("Eurostat_Transport")
  #pkm/yr
  x4 <- x4/10^9
  #BpKm/yr
  
  x <- x[,Reduce(intersect, list(getYears(x),getYears(x4)))]
  x4 <- x4[,Reduce(intersect, list(getYears(x),getYears(x4)))]
  
  x4[x4 < 0.1] <- NA
  
  PA <- (x/x4)
  
  PA <- PA[,,"PA"]
  
  x6 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-road")
  #million tKm/yr
  x6 <-x6/1000
  #Billion tKm/yr
  
  x <- x[,Reduce(intersect, list(getYears(x),getYears(x6)))]
  x6 <- x6[,Reduce(intersect, list(getYears(x),getYears(x6)))]
  
  x6[x6 < 0.1] <- NA
  
  GU <- (x/x6)
  
  GU <- GU[,,"GU"]
  
  x7 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-rail")
  #million tKm/yr
  x7 <- x7/1000
  #Billion tKm/yr
  
  x <- x[,Reduce(intersect, list(getYears(x),getYears(x7)))]
  x7 <- x7[,Reduce(intersect, list(getYears(x),getYears(x7)))]
  
  x7[x7 < 0.1] <- NA
  
  GT <- (x/x7)
  
  GT <- GT[,,"GT"]
  
  x8 <- readSource("IRF", subtype = "inland-surface-freight-transport-by-inland-waterway")
  #million tKm/yr
  x8 <- x8/1000
  #Billion tKm/yr
  
  x <- x[,Reduce(intersect, list(getYears(x),getYears(x8)))]
  x8 <- x8[,Reduce(intersect, list(getYears(x),getYears(x8)))]
  
  x8[x8 < 0.1] <- NA
  
  GN <- (x/x8)
  
  GN <- GN[,,"GN"]
  
  PA <- as.quitte(PA)
  PC <- as.quitte(PC)
  PT <- as.quitte(PT)
  GT <- as.quitte(GT)
  GU <- as.quitte(GU)
  GN <- as.quitte(GN)
  #ktoe/Bpkm
  
  PA <- select((PA), -c(unit1, unit))
  PC <- select((PC), -c(variable1,unit1, unit))
  PT <- select((PT), -c(variable1,unit1, unit))
  GN <- select((GN), -c(variable1,unit1, unit))
  GU <- select((GU), -c(variable1,unit1, unit))
  GT <- select((GT), -c(variable1,unit1, unit))

  y <- rbind(PC, PT, PA, GU, GT, GN)
  
  qx <- as.quitte(y) %>% as.magpie()
  
  qx <- as.quitte(qx) 
  qx_bu<- qx
  # assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv")
  names(qx) <- sub("region", "CountryCode", names(qx))
  ## add h12 mapping to dataset
  qx <- left_join(qx, h12, by="CountryCode")
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
       unit = "ktoe/Bpkm, ktoe/Btkm",
       description = "iInitSpecFuelCons")
  
}