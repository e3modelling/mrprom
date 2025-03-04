#' calcIH2Production
#'
#' Use technology cost data from the "EU Reference Scenario", MENA_EDS model data,
#' Technical Lifetime, CR and AVAIL data from various sources to derive OPENPROM
#' input parameter IH2Production.
#'
#' @return  OPENPROM input data H2production.
#' The output data for Investment cost per unit of capacity (IC) from
#' technology cost and other data from the "EU Reference Scenario".
#' The output data for Fixed Costs (FC) and (EFF)  is from "EU Reference Scenario".
#' The output data for Variable Costs (VC), (AVAIL), (LFT) and (CR) is from excel
#' Common_DATA from MENA_EDS model.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IH2Production", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr intersect %>% filter select
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIH2Production <- function() {
  
  #Capital Costs (IC)
  a <- readSource("TechCosts", subtype = "new_fuels_energy")

  years <- getYears(a)
  years <- sub("y", "", years)
  years <- as.numeric(years)
  
  q <- as.quitte(a)
  
  H2TTECH <- c("GSR", "WEG", "GSS")
  
  #make dataframe with all the available variables
  x <- as.data.frame(expand.grid(H2TTECH, years, c("IC", "FC", "EFF")))
  
  names(x) <- c("H2TTECH", "period", "variable")
  
  x["value"] <- NA
  
  #IC
  x[which(x["H2TTECH"] == "GSR" & x["variable"] == "IC"), 4] <- q[which(q["technologies"] == "Hydrogen from natural gas steam reforming centralised - Large scale  (per 1 kW or 1 MWh H2 HHV)" &
                                                  q["variable"] == "Investment cost per unit of capacity (EUR/kW-output)"), "value"]

  x[which(x["H2TTECH"] == "GSS" & x["variable"] == "IC"), 4] <- q[which(q["technologies"] == "Hydrogen from natural gas steam reforming centralised - Large scale with CCU  (per 1 kW or 1 MWh H2 HHV)" &
                                                  q["variable"] == "Investment cost per unit of capacity (EUR/kW-output)"), "value"]
  
  x[which(x["H2TTECH"] == "WEG" & x["variable"] == "IC"), 4] <- q[which(q["technologies"] == "Hydrogen from low temperature water electrolysis - Alkaline centralised, large scale  (per 1 kW or 1 MWh H2 HHV)" &
                                                  q["variable"] == "Investment cost per unit of capacity (EUR/kW-output)"), "value"]
  
  x[which(x["H2TTECH"] == "BGFLS" & x["variable"] == "IC"), 4] <- q[which(q["technologies"] == "Hydrogen from low temperature water electrolysis - Alkaline centralised, large scale  (per 1 kW or 1 MWh H2 HHV)" &
                                                                          q["variable"] == "Investment cost per unit of capacity (EUR/kW-output)"), "value"]
  
  x[which(x["variable"] == "IC"), 4] <- x[which(x["variable"] == "IC"), 4] * 1.1 #EUR2015 to USD2015
  
  #FC
  x[which(x["H2TTECH"] == "GSR" & x["variable"] == "FC"), 4] <- q[which(q["technologies"] == "Hydrogen from natural gas steam reforming centralised - Large scale  (per 1 kW or 1 MWh H2 HHV)" &
                                                  q["variable"] == "Fixed O&M costs\r\n(EUR/kW-output)"), "value"]
  
  x[which(x["H2TTECH"] == "GSS" & x["variable"] == "FC"), 4] <- q[which(q["technologies"] == "Hydrogen from natural gas steam reforming centralised - Large scale with CCU  (per 1 kW or 1 MWh H2 HHV)" &
                                                  q["variable"] == "Fixed O&M costs\r\n(EUR/kW-output)"), "value"]
  
  x[which(x["H2TTECH"] == "WEG" & x["variable"] == "FC"), 4] <- q[which(q["technologies"] == "Hydrogen from low temperature water electrolysis - Alkaline centralised, large scale  (per 1 kW or 1 MWh H2 HHV)" &
                                                  q["variable"] == "Fixed O&M costs\r\n(EUR/kW-output)"), "value"]
  
  x[which(x["variable"] == "FC"), 4] <- x[which(x["variable"] == "FC"), 4] * 1.1 #EUR2015 to USD2015
  
  #EFF %
  x[which(x["H2TTECH"] == "GSR" & x["variable"] == "EFF"), 4] <- q[which(q["technologies"] == "Hydrogen from natural gas steam reforming centralised - Large scale  (per 1 kW or 1 MWh H2 HHV)" &
                                                  q["variable"] == "Heatrate\r\n(input over output ratio)"), "value"]
  
  x[which(x["H2TTECH"] == "GSS" & x["variable"] == "EFF"), 4] <- q[which(q["technologies"] == "Hydrogen from natural gas steam reforming centralised - Large scale with CCU  (per 1 kW or 1 MWh H2 HHV)" &
                                                  q["variable"] == "Heatrate\r\n(input over output ratio)"), "value"]
  
  x[which(x["H2TTECH"] == "WEG" & x["variable"] == "EFF"), 4] <- q[which(q["technologies"] == "Hydrogen from low temperature water electrolysis - Alkaline centralised, large scale  (per 1 kW or 1 MWh H2 HHV)" &
                                                  q["variable"] == "Heatrate\r\n(input over output ratio)"), "value"]
  
  
  #BGFLS from excel Common_DATA
  #VC,AVAIL, LFT, CR from excel Common_DATA
  #make dataframe with all the available variables
  H2TTECH <- c("BGFLS")
  k <- as.data.frame(expand.grid(H2TTECH, c(2000,2025,2050), c("IC", "FC", "EFF")))
  names(k) <- c("H2TTECH", "period", "variable")
  k["value"] <- NA
  k[which(k["H2TTECH"] == "BGFLS" & k["variable"] == "IC"), 4] <- c(305,143,130) * 1.3 #EUR2005 to USD2015
  k[which(k["H2TTECH"] == "BGFLS" & k["variable"] == "FC"), 4] <- c(9.3,7.0,6.3) * 1.3 #EUR2005 to USD2015
  k[which(k["H2TTECH"] == "BGFLS" & k["variable"] == "EFF"), 4] <- c(60,63,65)
  
  
  H2TTECH <- c("GSR", "WEG", "GSS", "BGFLS")
  y <- as.data.frame(expand.grid(H2TTECH, c(2000,2025,2050), c("VC", "AVAIL")))
  names(y) <- c("H2TTECH", "period", "variable")
  y["value"] <- NA
  
  y[which(y["H2TTECH"] == "GSR" & y["variable"] == "VC"), 4] <- c(11.7,10.5,9.5)
  y[which(y["H2TTECH"] == "GSS" & y["variable"] == "VC"), 4] <- c(29.7,27.5,25.5)
  y[which(y["H2TTECH"] == "WEG" & y["variable"] == "VC"), 4] <- c(0.0,0.0,0.0)
  y[which(y["H2TTECH"] == "BGFLS" & y["variable"] == "VC"), 4] <- c(18.0,17.0,16.0)
  #AVAIL %
  y[which(y["H2TTECH"] == "GSR" & y["variable"] == "AVAIL"), 4] <- c(74,78,81)
  y[which(y["H2TTECH"] == "GSS" & y["variable"] == "AVAIL"), 4] <- c(62,68,73)
  y[which(y["H2TTECH"] == "WEG" & y["variable"] == "AVAIL"), 4] <- c(70,75,80)
  y[which(y["H2TTECH"] == "BGFLS" & y["variable"] == "AVAIL"), 4] <- c(90,90,90)
  
  z <- as.data.frame(expand.grid(H2TTECH,2020, c("LFT", "CR")))
  names(z) <- c("H2TTECH", "period", "variable")
  z["value"] <- NA
  
  z[which(z["H2TTECH"] == "GSR" & z["variable"] == "LFT"), 4] <- 25
  z[which(z["H2TTECH"] == "GSS" & z["variable"] == "LFT"), 4] <- 25
  z[which(z["H2TTECH"] == "WEG" & z["variable"] == "LFT"), 4] <- 20
  z[which(z["H2TTECH"] == "BGFLS" &z["variable"] == "LFT"), 4] <- 25
  
  z[which(z["H2TTECH"] == "GSS" & z["variable"] == "CR"), 4] <- 0.89
  z[which(z["H2TTECH"] == "BGFLS" & z["variable"] == "CR"), 4] <- 0.89
  
 qx <- rbind(x, k, y, z)
 
  x <- as.quitte(qx) %>%
    interpolate_missing_periods(period = 2010:2100, expand.values = TRUE)

  x <- as.magpie(x)

  #select years
  x <- x[,2010:2100,]
  
  # set NA to 0
  x[is.na(x)] <- 0

  return(list(x = x,
              weight = NULL,
              unit = "various",
              description = "readTechCosts;EU Reference Scenario and MENA_EDS"))

}
