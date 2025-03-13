#' readIEA_WEO_TechCosts
#'
#' Read PG_Assumptions TechCosts from the IEA World Energy Outlook 2023 global
#' source of energy analysis and projections.
#' The  Scenarios are Stated Policies and Net Zero Emissions by 2050.
#'
#' @param subtype Type of data that should be read.
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_WEO_TechCosts", subtype = "FuelPrices")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr %>% select mutate rename
#' @importFrom tidyr pivot_longer spread gather
#' @importFrom quitte as.quitte
#'
readIEA_WEO_TechCosts <- function(subtype = "Renewables") {
  
  # Read the Excel file
  data <- read_excel("WEO_2023_PG_Assumptions_STEPSandNZE_Scenario.xlsx", sheet = "Renewables")
  data <- data[-c(1, 2, 3, 6),]
  # data[1, 1 : 21] <- "Stated Policies"
  # data[1, 21: 35] <- "Net Zero Emissions by 2050"
  data <- data[,-c(5, 9, 13, 17, 21, 24, 27, 30, 33)]
  names(data) <- data[3,]
  names(data)[1] <- "region"
  data <- data[-c(1, 2, 3),]
  data <- data[-seq(from = 1, to = nrow(data), by = 10),]
  
  data <- data %>% pivot_longer(!region, names_to = "period", values_to = "value")
  
  data["technology"] <- NA
  data[c(seq(from = 1, to = nrow(data), by = 25),seq(from = 2, to = nrow(data),
  by = 25), seq(from = 3, to = nrow(data),by = 25),seq(from = 16, to = nrow(data),
  by = 25),seq(from = 17, to = nrow(data),by = 25)), 4] <- "Capital costs (USD/kW)"
  
  data[c(seq(from = 4, to = nrow(data), by = 25),seq(from = 5, to = nrow(data),
  by = 25), seq(from = 6, to = nrow(data),by = 25),seq(from = 18, to = nrow(data),
  by = 25),seq(from = 19, to = nrow(data),by = 25)), 4] <- "Annual O&M Costs (USD/kW)"
  
  data[c(seq(from = 7, to = nrow(data), by = 25),seq(from = 8, to = nrow(data),
  by = 25), seq(from = 9, to = nrow(data),by = 25),seq(from = 20, to = nrow(data),
  by = 25),seq(from = 21, to = nrow(data),by = 25)), 4] <- "Efficiency (gross, LHV)"
  
  data[c(seq(from = 10, to = nrow(data), by = 25),seq(from = 11, to = nrow(data),
  by = 25), seq(from = 12, to = nrow(data),by = 25),seq(from = 22, to = nrow(data),
  by = 25),seq(from = 23, to = nrow(data),by = 25)), 4] <- "Capacity factor (%)"
  
  data[c(seq(from = 13, to = nrow(data), by = 25),seq(from = 14, to = nrow(data),
  by = 25), seq(from = 15, to = nrow(data),by = 25),seq(from = 24, to = nrow(data),
  by = 25),seq(from = 25, to = nrow(data),by = 25)), 4] <- "Construction Time (years)"

  data["variable"] <- NA
  data[1 : 225, 5] <- "Solar photovoltaics - Large scale unit"
  data[226 : 450, 5] <- "Solar photovoltaics - Buildings"
  data[451 : 675, 5] <- "Wind onshore"
  data[676 : 900, 5] <- "Wind offshore"
  data[901 : 1125, 5] <- "Hydropower - large-scale unit"
  data[1126 : 1350, 5] <- "Hydropower - small-scale unit"
  data[1351 : 1575, 5] <- "Bioenergy - Large scale unit"
  data[1576 : 1800, 5] <- "Bioenergy - Cofiring"
  data[1801 : 2025, 5] <- "Bioenergy - Medium-scale CHP"
  data[2026 : 2250, 5] <- "Bioenergy + CCUS"
  data[2251 : 2475, 5] <- "Concentrating solar power"
  data[2476 : 2700, 5] <- "Geothermal"
  data[2701 : 2925, 5] <- "Marine"
  

  data["scenario"] <-  NA
  data[2701 : 2925, 5] <- "Stated Policies"
  data[2701 : 2925, 5] <- "Net Zero Emissions by 2050"
  
  # Convert to magpie object
  x <- as.quitte(long_data) %>% as.magpie()
  
  # Converting the values from $2022 to $2015
  # assuming a cumulative price change of -19%
  x <- x * 0.81
  
  # Converting the crude oil price from $/Barrel to $/toe
  x[, , "IEA crude oil"] <- x[, , "IEA crude oil"] * 7.2
  
  # Converting the natural gas price from $/MBtu to $/toe
  x[, , "Natural gas average"] <-  x[, , "Natural gas average"] * 39.652
  
  # Converting the coal price from $/tn to $/toe
  x[, , "Steam coal average"] <- x[, , "Steam coal average"] * 1.666
  
  # Setting the new unit for all rows
  xq <- as.quitte(x)
  xq[["unit"]] <- "$2015/toe"
  xq[["unit"]] <- factor(xq[["unit"]])
  
  x <- as.magpie(xq)
  
  list(x = x,
       weight = NULL,
       description = c(category = "Costs",
                       type = "Read PG_Assumptions TechCosts from the IEA World Energy Outlook 2023",
                       filename = "WEO_2023_PG_Assumptions_STEPSandNZE_Scenario.xlsx",
                       `Indicative size (MB)` = 0.012,
                       dimensions = "2D",
                       unit = "$2015/toe",
                       Confidential = "E3M"))
}
