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
  
  y["fuel"] <- y["new"]
  
  z_PC <- data.frame(matrix(data = NA, nrow = nrow(unique(y["region"]))*6, ncol = length(y)))
  names(z_PC) <- names(y)
  l <- data.frame(filter(y, variable == "PC", fuel == "GSL"))
  z_PC[, 1] <- levels(factor(l[, 1]))
  z_PC[, 2] <- levels(factor(l[, 2]))
  z_PC[, 3] <- levels(factor(l[, 3]))
  z_PC[, 4] <- levels(factor(l[, 4]))
  z_PC[, 5] <- levels(factor(l[, 5]))
  z_PC[1 : nrow(unique(y["region"])), 6] <- 0.6*(l[, 6])
  z_PC[1 : nrow(unique(y["region"])), 7] <- "CHEVGSL"
  z_PC[1 : nrow(unique(y["region"])), 8] <- "GSL"
  
  l <- data.frame(filter(y, variable == "PC", fuel == "GDO"))
  z_PC[(nrow(unique(y["region"])) + 1) : (nrow(unique(y["region"]))*2), 6] <- 0.6*(l[, 6])
  z_PC[(nrow(unique(y["region"])) + 1) : (nrow(unique(y["region"]))*2), 7] <- "CHEVGDO"
  z_PC[(nrow(unique(y["region"])) + 1) : (nrow(unique(y["region"]))*2), 8] <- "GDO"

  l <- data.frame(filter(y, variable == "PC", fuel == "GSL"))
  z_PC[(nrow(unique(y["region"]))*2 + 1) : (nrow(unique(y["region"]))*3), 6] <- 0.6*(l[, 6])
  z_PC[(nrow(unique(y["region"]))*2 + 1) : (nrow(unique(y["region"]))*3), 7] <- "PHEVGSL"
  z_PC[(nrow(unique(y["region"]))*2 + 1) : (nrow(unique(y["region"]))*3), 8] <- "GSL"
  
  l <- data.frame(filter(y, variable == "PC", fuel == "ELC"))
  z_PC[(nrow(unique(y["region"]))*3 + 1) : (nrow(unique(y["region"]))*4), 6] <- (l[, 6])
  z_PC[(nrow(unique(y["region"]))*3 + 1) : (nrow(unique(y["region"]))*4), 7] <- "PHEVGSL"
  z_PC[(nrow(unique(y["region"]))*3 + 1) : (nrow(unique(y["region"]))*4), 8] <- "ELC"
  
  l <- data.frame(filter(y, variable == "PC", fuel == "GDO"))
  z_PC[(nrow(unique(y["region"]))*4 + 1) : (nrow(unique(y["region"]))*5), 6] <- 0.6*(l[, 6])
  z_PC[(nrow(unique(y["region"]))*4 + 1) : (nrow(unique(y["region"]))*5), 7] <- "PHEVGDO"
  z_PC[(nrow(unique(y["region"]))*4 + 1) : (nrow(unique(y["region"]))*5), 8] <- "GDO"
  
  l <- data.frame(filter(y, variable == "PC", fuel == "ELC"))
  z_PC[(nrow(unique(y["region"]))*5 + 1) : (nrow(unique(y["region"]))*6), 6] <- (l[, 6])
  z_PC[(nrow(unique(y["region"]))*5 + 1) : (nrow(unique(y["region"]))*6), 7] <- "PHEVGDO"
  z_PC[(nrow(unique(y["region"]))*5 + 1) : (nrow(unique(y["region"]))*6), 8] <- "ELC"
  
  z_GU <- data.frame(matrix(data = NA, nrow = nrow(unique(y["region"]))*5, ncol = length(y)))
  names(z_GU) <- names(y)
  l <- data.frame(filter(y, variable == "GU", fuel == "GDO"))
  z_GU[, 1] <- levels(factor(l[, 1]))
  z_GU[, 2] <- levels(factor(l[, 2]))
  z_GU[, 3] <- levels(factor(l[, 3]))
  z_GU[, 4] <- levels(factor(l[, 4]))
  z_GU[, 5] <- levels(factor(l[, 5]))
  z_GU[1 : nrow(unique(y["region"])), 6] <- 0.6*(l[, 6])
  z_GU[1 : nrow(unique(y["region"])), 7] <- "CHEVGDO"
  z_GU[1 : nrow(unique(y["region"])), 8] <- "GDO"
  
  l <- data.frame(filter(y, variable == "GU", fuel == "GSL"))
  z_GU[(nrow(unique(y["region"])) + 1) : (nrow(unique(y["region"]))*2), 6] <- 0.6*(l[, 6])
  z_GU[(nrow(unique(y["region"])) + 1) : (nrow(unique(y["region"]))*2), 7] <- "PHEVGSL"
  z_GU[(nrow(unique(y["region"])) + 1) : (nrow(unique(y["region"]))*2), 8] <- "GSL"
  
  l <- data.frame(filter(y, variable == "GU", fuel == "ELC"))
  z_GU[(nrow(unique(y["region"]))*2 + 1) : (nrow(unique(y["region"]))*3), 6] <- l[, 6]
  z_GU[(nrow(unique(y["region"]))*2 + 1) : (nrow(unique(y["region"]))*3), 7] <- "PHEVGSL"
  z_GU[(nrow(unique(y["region"]))*2 + 1) : (nrow(unique(y["region"]))*3), 8] <- "ELC"
  
  l <- data.frame(filter(y, variable == "GU", fuel == "GDO"))
  z_GU[(nrow(unique(y["region"]))*3 + 1) : (nrow(unique(y["region"]))*4), 6] <- 0.6*(l[, 6])
  z_GU[(nrow(unique(y["region"]))*3 + 1) : (nrow(unique(y["region"]))*4), 7] <- "PHEVGDO"
  z_GU[(nrow(unique(y["region"]))*3 + 1) : (nrow(unique(y["region"]))*4), 8] <- "GDO"
  
  l <- data.frame(filter(y, variable == "GU", fuel == "ELC"))
  z_GU[(nrow(unique(y["region"]))*4 + 1) : (nrow(unique(y["region"]))*5), 6] <- l[, 6]
  z_GU[(nrow(unique(y["region"]))*4 + 1) : (nrow(unique(y["region"]))*5), 7] <- "PHEVGDO"
  z_GU[(nrow(unique(y["region"]))*4 + 1) : (nrow(unique(y["region"]))*5), 8] <- "ELC"
  
  y <- rbind(y, z_PC, z_GU)
  
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
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "new", "variable", "fuel"))
  names(qx) <- sub("CountryCode", "region", names(qx))
  qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  qx_bu <- select(qx_bu, -c("model", "scenario"))
  ## assign to countries with NA, their H12 region mean
  value.x <- NULL
  value.y <- NULL
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "new", "unit", "fuel")) %>% 
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>% 
    select(-c("value.x", "value.y"))
  ## assign to countries that still have NA, the global mean
  qx_bu <- qx
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "new", "variable", "fuel"))
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "new", "unit", "fuel")) %>% 
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