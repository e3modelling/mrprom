#' calcIDataTransTech
#'
#' Use technology cost data from the "EU Reference Scenario", MENA_EDS model data,
#' and Technical Lifetime data from various sources to derive OPENPROM input parameter
#' iDataTransTech.
#'
#' @return  OPENPROM input data iDataTransTech
#' The output data for Capital Costs (IC) per vehicle calculated from
#' technology cost and other data from the "EU Reference Scenario".
#' The output data for Fixed Costs (FC) per vehicle is from MENA_EDS model.
#' The output data for Technical Lifetime (LFT) is from US Department of
#' Transportation, International Union of Railways, Statista, EU CORDIS.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataTransTech", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr intersect %>% filter select
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom magclass getItems

calcIDataTransTech <- function() {
  
  #Capital Costs (IC)
  a1 <- readSource("TechCosts2024", subtype = "Medium_cars") #Euro'15 /veh, 
  a2 <- readSource("TechCosts2024", subtype = "Rail") #Million Euro'15 , calcACTV : million vehicles
  a3 <- readSource("TechCosts2024", subtype = "Aviation") #Million Euro'15, calcACTV : million vehicles
  a4 <- readSource("TechCosts2024", subtype = "Inland_navigation") #Million Euro'15 /veh, calcACTV : million vehicles
  a5 <- readSource("TechCosts2024", subtype = "HGVs>16t") #trucks Euro'15 /veh, calcACTV : million vehicles
  a6 <- readSource("TechCosts2024", subtype = "Bus_coach") #buses Euro'15 /veh, calcACTV : million vehicles
  
  q <- mbind(a1, a2, a3, a4, a5, a6)
  years <- getYears(q)
  years <- sub("y", "", years)
  years <- as.numeric(years)
  q <- as.quitte(q)
  # efficiency_value from character to number
  q[["efficiency_value"]] <- sub("_", ".", q[["efficiency_value"]])
  q["efficiency_value"] <- as.numeric(unlist(q["efficiency_value"]))

  map <- toolGetMapping(name = "iDataTransTech-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")
  # load current OPENPROM set configuration
  TRANSFINAL <- toolGetMapping(name = "TRANSE.csv",
                         type = "blabla_export",
                         where = "mrprom")
  
  TRANSFINAL <- as.character(TRANSFINAL[, 1])

  TTECH <- toolGetMapping(name = "TTECH.csv",
                               type = "blabla_export",
                               where = "mrprom")
  
  TTECH <- as.character(TTECH[, 1])
  
  #make dataframe with all the available variables
  x <- as.data.frame(expand.grid(TTECH, TRANSFINAL, years))
  
  #take the median value, if even number take the first one from the two medians
  mymedian <- function(lst) {
    n <- length(lst)
    s <- sort(lst)
    ifelse(n%%2==1, s[(n + 1) / 2], (s[n / 2]))
  }
  efficiency_value <- NULL
  q <- mutate(q, mean_of_eff = mymedian(efficiency_value), .by = c("variable"))
  
  #find the index of each variable and assigned it to the corresponding variable
  for (i in 1 : nrow(map)) {
    index11 <- which(x["Var2"] == map[i, 2] & x["Var1"] == map[i, 1] & x["Var3"] != 2015)
    index5 <- which(q["variable"] == map[i, 3])
    index8 <- which(q["efficiency_value"] == q["mean_of_eff"] & q["variable"] == map[i, 3] & !(is.na(q["value"])))
    index9 <- which(q["variable"] == map[i, 3] & q["period"] == 2015 & !(is.na(q["value"])))
    index13 <- which(x["Var3"] == 2015 & x["Var2"] == map[i, 2] & x["Var1"] == map[i, 1])

    if (length(index9) == 0) {
      index13 <- NULL
    }
    if (length(index9) == 0) {
      index9 <- NA
    }
    if (length(index13) == 0) {
      index9 <- NULL
    }
    if (length(index5) == 0) {
      index13 <- NULL
      index11 <- NULL
      index9 <- NULL
      index8 <- NULL
    }
    if (length(index8) == 3) {
      index11 <- index11[-1]
    }
    x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  }

  names(x) <- c("TTECH", "TRANSFINAL" , "period", "value")

  x["variable"] <- "IC"

  #fix units to kEuro'15
  x[which(x["TRANSFINAL"] == "PC" & x["variable"] == "IC"), 4] <- x[which(x["TRANSFINAL"] == "PC" & x["variable"] == "IC"), 4] / 1000
  x[which(x["TRANSFINAL"] == "PB" & x["variable"] == "IC"), 4] <- x[which(x["TRANSFINAL"] == "PB" & x["variable"] == "IC"), 4] / 1000
  x[which(x["TRANSFINAL"] == "GU" & x["variable"] == "IC"), 4] <- x[which(x["TRANSFINAL"] == "GU" & x["variable"] == "IC"), 4] / 1000
  x[which(x["TRANSFINAL"] == "PT" & x["variable"] == "IC"), 4] <- x[which(x["TRANSFINAL"] == "PT" & x["variable"] == "IC"), 4] * 1000
  x[which(x["TRANSFINAL"] == "GT" & x["variable"] == "IC"), 4] <- x[which(x["TRANSFINAL"] == "GT" & x["variable"] == "IC"), 4] * 1000
  x[which(x["TRANSFINAL"] == "GN" & x["variable"] == "IC"), 4] <- x[which(x["TRANSFINAL"] == "GN" & x["variable"] == "IC"), 4] * 1000
  x[which(x["TRANSFINAL"] == "PN" & x["variable"] == "IC"), 4] <- x[which(x["TRANSFINAL"] == "PN" & x["variable"] == "IC"), 4] * 1000
  x[which(x["TRANSFINAL"] == "PA" & x["variable"] == "IC"), 4] <- x[which(x["TRANSFINAL"] == "PA" & x["variable"] == "IC"), 4] * 1000

  #Fixed Costs (FC) from MENA_EDS
  ECONCHAR <- NULL
  EF <- NULL
  a <- readSource("MENA_EDS", subtype = "Trans_Tech")
  a <- as.quitte(a)
  a <- filter(a, ECONCHAR %in% c("FC_05", "FC_25", "FC_50"))
  a <- filter(a, EF %in% c("GSL", "LPG", "GDO", "NGS", "ELC", "KRS", "ETH", "MET",
                           "H2F", "BGDO", "PHEVGSL", "PHEVGDO", "CHEVGSL", "CHEVGDO"))
  a["variable"] <- "FC"
  a[["ECONCHAR"]] <- sub("FC_", 20, a[["ECONCHAR"]])
  a["period"] <- a["ECONCHAR"]
  a <- select((a), -c(ECONCHAR))
  names(a)[9] <- "ttech"
  names(a)[8] <- "transfinal"
  PB <- a[which(a["transfinal"] == "PC"), ]
  PB[,"transfinal"] <- "PB"
  PB <- PB %>% filter(!(ttech %in% c("CHEVGSL", "CHEVGDO")))
  PN <- a[which(a["transfinal"] == "GN"), ]
  PN[,"transfinal"] <- "PN"
  a <- rbind(a, PB, PN)
  
  #VC is 0
  x <- as.quitte(x)
  vc <- x
  vc["variable"] <- "VC"
  vc["value"] <- 0
  x <- rbind(x, a, vc)

  #keep only the correlations that are needed
  ttech <- NULL
  transfinal <- NULL
  x <- x %>% filter(!(ttech == "KRS" & transfinal == "PC"))
  
  x <- x %>% filter(!(ttech == "KRS" & transfinal == "PB"))

  x <- x %>% filter(!((ttech %in% c("GSL", "LPG", "NGS", "KRS", "ETH", "CHEVGDO", "BGDO", "PHEVGSL",
                                    "PHEVGDO", "CHEVGSL")) & transfinal == "PT"))

  x <- x %>% filter(!((ttech %in% c("GSL", "LPG", "NGS", "GDO", "ELC", "ETH", "MET",
                                    "BGDO", "PHEVGSL", "PHEVGDO", "CHEVGSL", "CHEVGDO")) & transfinal == "PA"))

  x <- x %>% filter(!((ttech %in% c("KRS", "CHEVGSL")) & transfinal == "GU"))

  x <- x %>% filter(!((ttech %in% c("GSL", "LPG", "NGS", "KRS", "ETH", "BGDO",
                                    "PHEVGSL", "PHEVGDO", "CHEVGSL", "CHEVGDO")) & transfinal == "GT"))

  x <- x %>% filter(!((ttech %in% c("LPG", "NGS", "ELC", "KRS", "ETH", "MET",
                                    "BGDO", "PHEVGSL", "PHEVGDO", "CHEVGSL", "CHEVGDO")) & transfinal == "GN"))

  x <- x %>% filter(!((ttech %in% c("LPG", "NGS", "ELC", "KRS", "ETH", "MET",
                                    "BGDO", "PHEVGSL", "PHEVGDO", "CHEVGSL", "CHEVGDO")) & transfinal == "PN"))
  

  x[["period"]] <- as.integer(x[["period"]])
  
  x <- interpolate_missing_periods(x, period = 2010:2100, expand.values = TRUE)

  period <- NULL
  x <- filter(x, period != 2005)
  
  #lifetimes for Transport sector
  b <- readSource("LifetimesTranstech")
  b <- as.quitte(b)
  
  #b <- filter(b, transfinal %in% c("PC", "PA", "PT", "GU", "GT", "GN"))
  b["variable"] <- "LFT"
  b["period"] <- 2010
  b <- as.quitte(b) %>%
    interpolate_missing_periods(period = 2010:2100, expand.values = TRUE)
  x <- rbind(x, b)

  x <- as.magpie(x)
  
  # Converting EUR TO USD
  x[, , "IC"] <-  x[, , "IC"] * 1.1 #EUR2015 to USD2015
  x[, , "FC"] <-  x[, , "FC"] * 1.3 #EUR2005 to USD2015
  x[, , "VC"] <-  x[, , "VC"] * 1.1

  # set NA to 0
  x[is.na(x)] <- 0
  getItems(x, 3.2) <- paste0("T", getItems(x, 3.2))

  ######### activity units
  
  # PC calcACTV : million vehicles, techCosts IC : #USD2015 /veh
  
  x[,,"PC"] <- x[,,"PC"] / 1000 #thousand
  
  # PB calcACTV : Billion pKm/yr, techCosts IC : #USD2015 /veh
  
  x[,,"PB"] <- x[,,"PB"] * 1.5 * 10^9 * (1/10^9) / 1000 #1.5 million pkm / billion / thousand dollars
  
  # PT calcACTV : Billion pKm/yr, techCosts IC : #Million USD2015 /veh
  
  x[,,"PT"] <- x[,,"PT"] * 10^9 * 87.5 * 10^6 * (1/10^9) / 1000 #87.5 million pkm * million USD2015 / billion / thousand dollars
  
  # PA calcACTV : million passengers, techCosts IC : #Million USD2015 /veh
  
  x[,,"PA"] <- x[,,"PA"] * 10^6 * 250 * 10^6 * (1/10^9) / 1000 #250 million pkm * million USD2015 / billion / thousand dollars
  
  # GU calcACTV : GtKm/yr, techCosts IC : #USD2015 /veh
  
  x[,,"GU"] <- x[,,"GU"] * 0.0004 / 1000 #0.0004 GtKm / thousand dollars
  
  # GT calcACTV : GtKm/yr, techCosts IC : #Million USD2015
  
  x[,,"GT"] <- x[,,"GT"] * 0.0548 * 10^6 / 1000 #0.0548 GtKm * million USD2015 / thousand dollars
  
  # GN calcACTV : GtKm/yr, techCosts IC : #Million USD2015 /veh
  
  x[,,"GN"] <- x[,,"GN"] * 0.000008 * 10^6 / 1000 #0.000008 GtKm * million USD2015 / thousand dollars
  
  # PN calcACTV : Billion pKm/yr, techCosts IC : #Million USD2015 /veh
  
  x[,,"PN"] <- x[,,"PN"] * 0.00000098 * 10^6 / 1000 #0.00000098 GtKm * million USD2015 / thousand dollars
  
  ##################
  
  return(list(x = x,
              weight = NULL,
              unit = "various",
              description = "readTechCosts;EU Reference Scenario and MENA_EDS"))

}
