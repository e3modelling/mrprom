#' calcISuppRatePrimProd
#'
#' Use ENERDATA data to derive OPENPROM input parameter iSuppRatePrimProd.
#'
#' @return  OPENPROM input data iSuppRatePrimProd.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "ISuppRatePrimProd", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte


calcISuppRatePrimProd <- function() {

  # load data source (ENERDATA)
  a <- calcOutput(type = "ISuppPrimprod", aggregate = FALSE)

  x <- a
  x[, , "HYD"] <- 1
  x[, , "BMSWAS"] <- 1
  x[, , "NUC"] <- 1
  x[, , "WND"] <- 1
  x[, , "SOL"] <- 1
  x[, , "GEO"] <- 1
  #CRO
  TREFINER <- readSource("IEA2025", subtype = "TREFINER")
  TREFINER <- TREFINER[,,"KTOE"]
  getItems(TREFINER,3.1) <- "Mtoe"
  TREFINER <- TREFINER / 1000 #ktoe to mtoe
  
  a1 <- TREFINER[, , c("NATURAL_GAS", "CRUDE_OIL")]
  
  TOTENGY <- readSource("IEA2025", subtype = "TOTENGY")
  TOTENGY <- TOTENGY[,,"KTOE"]
  getItems(TOTENGY,3.1) <- "Mtoe"
  TOTENGY <- TOTENGY / 1000 #ktoe to mtoe
  
  a2 <- TOTENGY[, , "CRUDE_OIL"]
  
  TRANSFERS <- readSource("IEA2025", subtype = "TRANSFERS")
  TRANSFERS <- TRANSFERS[,,"KTOE"]
  getItems(TRANSFERS,3.1) <- "Mtoe"
  TRANSFERS <- TRANSFERS / 1000 #ktoe to mtoe

  a3 <- TRANSFERS[, , "REFINERY_FEEDSTOCKS"]

  a1 <- a1[, Reduce(intersect, list(getYears(a1), getYears(a2), getYears(a3))), ]
  a2 <- a2[, Reduce(intersect, list(getYears(a1), getYears(a2), getYears(a3))), ]
  a3 <- a3[, Reduce(intersect, list(getYears(a1), getYears(a2), getYears(a3))), ]

  y <- mbind(a1, a2, a3)
  
  #sum of CRO (consumption of refineries input, own use, refinery feedstock)
  y <- dimSums(y, dim = 3, na.rm = TRUE)
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  y <- y[, c(max(fStartHorizon, min(getYears(y, as.integer = TRUE))) : max(getYears(y, as.integer = TRUE))), ]
  x <- x[, Reduce(intersect, list(getYears(x), getYears(y))), ]
  y <- y[, Reduce(intersect, list(getYears(x), getYears(y))), ]
 
  #ISuppPrimprod / sum of CRO
  x[, , "CRO"] <- x[, , "CRO"] / y
  #HCL
  n4 <- calcOutput(type = "IFuelCons", subtype = "NENSE", aggregate = FALSE)
  a4 <- dimSums(n4, dim = 3.1, na.rm = TRUE)
  a4 <- dimSums(a4, dim = 3.1, na.rm = TRUE)
  a4 <- a4[, , "HCL"]

  d5 <- calcOutput(type = "IFuelCons", subtype = "DOMSE", aggregate = FALSE)
  a5 <- dimSums(d5, dim = 3.1, na.rm = TRUE)
  a5 <- dimSums(a5, dim = 3.1, na.rm = TRUE)
  a5 <- a5[, , "HCL"]

  i11 <- calcOutput(type = "IFuelCons", subtype = "INDSE", aggregate = FALSE)
  a11 <- dimSums(i11, dim = 3.1, na.rm = TRUE)
  a11 <- dimSums(a11, dim = 3.1, na.rm = TRUE)
  a11 <- a11[, , "HCL"]
  
  ELOUTPUT <- readSource("IEA2025", subtype = "ELOUTPUT")
  ELOUTPUT <- ELOUTPUT[,,"GWH"]
  getItems(ELOUTPUT,3.1) <- "Mtoe"
  ELOUTPUT <- ELOUTPUT / 11630 #GWH to mtoe

  a12 <- ELOUTPUT[, , c("ANTHRACITE","OTH_BITCOAL","COKING_COAL")]

  a13 <- TOTENGY[, , c("ANTHRACITE","OTH_BITCOAL","COKING_COAL")]

  a4 <- a4[, Reduce(intersect, list(getYears(a4), getYears(a5), getYears(a11), getYears(a12), getYears(a13))), ]
  a5 <- a5[, Reduce(intersect, list(getYears(a4), getYears(a5), getYears(a11), getYears(a12), getYears(a13))), ]
  a11 <- a11[, Reduce(intersect, list(getYears(a4), getYears(a5), getYears(a11), getYears(a12), getYears(a13))), ]
  a12 <- a12[, Reduce(intersect, list(getYears(a4), getYears(a5), getYears(a11), getYears(a12), getYears(a13))), ]
  a13 <- a13[, Reduce(intersect, list(getYears(a4), getYears(a5), getYears(a11), getYears(a12), getYears(a13))), ]

  a12 <- dimSums(a12, dim = 3, na.rm = TRUE)
  getItems(a12,3) <- getItems(a4, 3)
  
  a13 <- dimSums(a13, dim = 3, na.rm = TRUE)
  getItems(a13,3) <- getItems(a4, 3)
  
  z <- mbind(a4, a5, a11, a12, a13)
  
  #sum of HCL(consumption of (NENSE, INDSE, DOMSE), own use, electricity sector)
  z <- dimSums(z, dim = 3, na.rm = TRUE)
  z <- z[, c(max(fStartHorizon, min(getYears(z, as.integer = TRUE))) : max(getYears(z, as.integer = TRUE))), ]
  x <- x[, Reduce(intersect, list(getYears(x), getYears(z))), ]
  z <- z[, Reduce(intersect, list(getYears(x), getYears(z))), ]

  #ISuppPrimprod / sum of HCL
  x[, , "HCL"] <- x[, , "HCL"] / z
  
  #ISuppPrimprod / sum of HCL
  n4_lgn <- dimSums(n4, dim = 3.1, na.rm = TRUE)
  n4_lgn <- dimSums(n4_lgn, dim = 3.1, na.rm = TRUE)
  n4_lgn <- n4_lgn[, , "LGN"]
  d5_lgn <- dimSums(d5, dim = 3.1, na.rm = TRUE)
  d5_lgn <- dimSums(d5_lgn, dim = 3.1, na.rm = TRUE)
  d5_lgn <- d5_lgn[, , "LGN"]
  a11_lgn <- dimSums(i11, dim = 3.1, na.rm = TRUE)
  a11_lgn <- dimSums(a11_lgn, dim = 3.1, na.rm = TRUE)
  a11_lgn <- a11_lgn[, , "LGN"]
  
  cons_lgn <- ELOUTPUT[, , c("LIGNITE","BKB","SUB_BITCOAL")]
  
  own_lgn <- TOTENGY[, , c("LIGNITE","BKB","SUB_BITCOAL")]
  
  n4_lgn <- n4_lgn[, Reduce(intersect, list(getYears(n4_lgn), getYears(d5_lgn), getYears(a11_lgn), getYears(cons_lgn), getYears(own_lgn))), ]
  d5_lgn <- d5_lgn[, Reduce(intersect, list(getYears(n4_lgn), getYears(d5_lgn), getYears(a11_lgn), getYears(cons_lgn), getYears(own_lgn))), ]
  a11_lgn <- a11_lgn[, Reduce(intersect, list(getYears(n4_lgn), getYears(d5_lgn), getYears(a11_lgn), getYears(cons_lgn), getYears(own_lgn))), ]
  cons_lgn <- cons_lgn[, Reduce(intersect, list(getYears(n4_lgn), getYears(d5_lgn), getYears(a11_lgn), getYears(cons_lgn), getYears(own_lgn))), ]
  own_lgn <- own_lgn[, Reduce(intersect, list(getYears(n4_lgn), getYears(d5_lgn), getYears(a11_lgn), getYears(cons_lgn), getYears(own_lgn))), ]
  
  cons_lgn <- dimSums(cons_lgn, dim = 3, na.rm = TRUE)
  getItems(cons_lgn,3) <- getItems(n4_lgn, 3)
  
  own_lgn <- dimSums(own_lgn, dim = 3, na.rm = TRUE)
  getItems(own_lgn,3) <- getItems(n4_lgn, 3)
  
  z_LGN <- mbind(n4_lgn, d5_lgn, a11_lgn, cons_lgn, own_lgn)
  
  #sum of HCL(consumption of (NENSE, INDSE, DOMSE), own use, electricity sector)
  z_LGN <- dimSums(z_LGN, dim = 3, na.rm = TRUE)
  z_LGN <- z_LGN[, c(max(fStartHorizon, min(getYears(z_LGN, as.integer = TRUE))) : max(getYears(z_LGN, as.integer = TRUE))), ]
  x <- x[, Reduce(intersect, list(getYears(x), getYears(z_LGN))), ]
  z_LGN <- z_LGN[, Reduce(intersect, list(getYears(x), getYears(z_LGN))), ]
  
  #ISuppPrimprod / sum of LGN
  x[, , "LGN"] <- x[, , "LGN"] / z_LGN

  #NGS
  a14 <- dimSums(n4, dim = 3.1, na.rm = TRUE)
  a14 <- dimSums(a14, dim = 3.1, na.rm = TRUE)
  a14 <- a14[, , "NGS"]

  a15 <- dimSums(d5, dim = 3.1, na.rm = TRUE)
  a15 <- dimSums(a15, dim = 3.1, na.rm = TRUE)
  a15 <- a15[, , "NGS"]

  a16 <- dimSums(i11, dim = 3.1, na.rm = TRUE)
  a16 <- dimSums(a16, dim = 3.1, na.rm = TRUE)
  a16 <- a16[, , "NGS"]

  t17 <- calcOutput(type = "IFuelCons", subtype = "TRANSE", aggregate = FALSE)
  a18 <- dimSums(t17, dim = 3.1, na.rm = TRUE)
  a18 <- dimSums(a18, dim = 3.1, na.rm = TRUE)
  a18 <- a18[, , "NGS"]

  a19 <- ELOUTPUT[, , "NATURAL_GAS"]

  a21 <- TOTENGY[, , "NATURAL_GAS"]

  DISTLOSS <- readSource("IEA2025", subtype = "DISTLOSS")
  DISTLOSS <- DISTLOSS[,,"KTOE"]
  getItems(DISTLOSS,3.1) <- "Mtoe"
  DISTLOSS <- DISTLOSS / 1000 #ktoe to mtoe
  
  a20 <- DISTLOSS[, , "NATURAL_GAS"]

  a14 <- a14[, Reduce(intersect, list(getYears(a14), getYears(a15), getYears(a16), getYears(a19), getYears(a21), getYears(a20), getYears(a18))), ]
  a15 <- a15[, Reduce(intersect, list(getYears(a14), getYears(a15), getYears(a16), getYears(a19), getYears(a21), getYears(a20), getYears(a18))), ]
  a16 <- a16[, Reduce(intersect, list(getYears(a14), getYears(a15), getYears(a16), getYears(a19), getYears(a21), getYears(a20), getYears(a18))), ]
  a19 <- a19[, Reduce(intersect, list(getYears(a14), getYears(a15), getYears(a16), getYears(a19), getYears(a21), getYears(a20), getYears(a18))), ]
  a21 <- a21[, Reduce(intersect, list(getYears(a14), getYears(a15), getYears(a16), getYears(a19), getYears(a21), getYears(a20), getYears(a18))), ]
  a20 <- a20[, Reduce(intersect, list(getYears(a14), getYears(a15), getYears(a16), getYears(a19), getYears(a21), getYears(a20), getYears(a18))), ]
  a18 <- a18[, Reduce(intersect, list(getYears(a14), getYears(a15), getYears(a16), getYears(a19), getYears(a21), getYears(a20), getYears(a18))), ]

  a21 <- dimSums(a21, dim = 3, na.rm = TRUE)
  getItems(a21,3) <- getItems(a18, 3)
  
  a20 <- dimSums(a20, dim = 3, na.rm = TRUE)
  getItems(a20,3) <- getItems(a18, 3)
  
  a19 <- dimSums(a19, dim = 3, na.rm = TRUE)
  getItems(a19,3) <- getItems(a18, 3)
  
  w <- mbind(a14, a15, a16, a19, a21, a20, a18)
  #sum of NGS (consumption in TRANSE, own use, electricity power plants, distribution losses)
  w <- dimSums(w, dim = 3, na.rm = TRUE)
  w <- w[, c(max(fStartHorizon, min(getYears(w, as.integer = TRUE))) : max(getYears(w, as.integer = TRUE))), ]
  x <- x[, Reduce(intersect, list(getYears(x), getYears(w))), ]
  w <- w[, Reduce(intersect, list(getYears(x), getYears(w))), ]
 
   #ISuppPrimprod / sum of NGS
  x[, , "NGS"] <- x[, , "NGS"] / w

  # complete incomplete time series
  qx <- as.quitte(x) %>%
  interpolate_missing_periods(period = 2010 : 2100, expand.values = TRUE)
  qx_bu <- qx
  # assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  names(qx) <- sub("region", "CountryCode", names(qx))
  ## add h12 mapping to dataset
  qx <- left_join(qx, h12, by = "CountryCode")
  ## add new column containing regional mean value
  value <- NULL
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "variable"))
  names(qx) <- sub("CountryCode", "region", names(qx))
  qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  qx_bu <- select(qx_bu, -c("model", "scenario"))
  ## assign to countries with NA, their H12 region mean
  value.x <- NULL
  value.y <- NULL
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  ## assign to countries that still have NA, the global mean
  qx_bu <- qx
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "variable"))
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  x <- as.quitte(qx) %>% as.magpie()

  # set INF to NA
  x[is.infinite(x)] <- NA
  x[is.na(x)] <- 0

  list(x = collapseNames(x),
       weight = NULL,
       unit = "Rate",
       description = "Enerdata;")

}
