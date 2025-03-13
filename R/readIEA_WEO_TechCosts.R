#' readIEA_WEO_TechCosts
#'
#' Read PG_Assumptions TechCosts from the IEA World Energy Outlook 2023 global
#' source of energy analysis and projections.
#' The  Scenarios are Stated Policies and Net Zero Emissions by 2050.
#'
#' @param subtype Type of data that should be read. The type is referring to the
#' excel sheet, from the excel file "WEO_2023_PG_Assumptions_STEPSandNZE_Scenario.xlsx"
#' and convert it to a magpie object.
#' Available types are:
#' \itemize{
#' \item `Renewables`:
#' \item `Nuclear`:
#' \item `Fossil fuels equipped with CCUS`:
#' \item `Coal`:
#' \item `Gas`:
#' }
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_WEO_TechCosts", subtype = "Renewables")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr %>% select
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readIEA_WEO_TechCosts <- function(subtype = "Renewables") {
  
  if (subtype == "Renewables") {
    # Read the Excel file
    data <- read_excel("WEO_2023_PG_Assumptions_STEPSandNZE_Scenario.xlsx", sheet = "Renewables")
    data <- data[-c(1, 2, 3, 6),]
    data <- data[,-c(5, 9, 13, 17, 21, 24, 27, 30, 33)]
    names(data) <- data[3,]
    names(data)[1] <- "region"
    data <- data[-c(1, 2, 3),]
    data <- data[-seq(from = 1, to = nrow(data), by = 10),]
    
    data <- data %>% pivot_longer(!region, names_to = "period", values_to = "value")
    
    data["variable"] <- NA
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
    
    data["technology"] <- NA
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
    
    for (i in 1:15) {
      data[c(seq(from = i, to = nrow(data), by = 25)), 6] <- "Stated Policies"
    }
    data[which(is.na(data[,6])), 6] <- "Net Zero Emissions by 2050"
    
    data[["value"]] <- as.numeric(data[["value"]])
    
    # Convert to magpie object
    x <- as.quitte(data) %>% as.magpie()
  }
  
  if (subtype == "Nuclear") {
    # Read the Excel file
    data <- read_excel("WEO_2023_PG_Assumptions_STEPSandNZE_Scenario.xlsx", sheet = "Nuclear")
    data <- data[-c(1:6,8),]
    data <- data[,-c(1 : 7, 12, 16, 20, 23, 26)]
    names(data) <- data[1,]
    names(data)[1] <- "region"
    data <- data[-c(1),]
    
    data <- data %>% pivot_longer(!region, names_to = "period", values_to = "value")
    
    data["variable"] <- NA
    data[c(seq(from = 1, to = nrow(data), by = 15),seq(from = 2, to = nrow(data),
    by = 15), seq(from = 3, to = nrow(data),by = 15),seq(from = 10, to = nrow(data),
    by = 15),seq(from = 11, to = nrow(data),by = 15)), 4] <- "Capital costs (USD/kW)"
    
    data[c(seq(from = 4, to = nrow(data), by = 15),seq(from = 5, to = nrow(data),
    by = 15), seq(from = 6, to = nrow(data),by = 15),seq(from = 12, to = nrow(data),
    by = 15),seq(from = 13, to = nrow(data),by = 15)), 4] <- "Annual O&M Costs (USD/kW)"
    
    data[c(seq(from = 7, to = nrow(data), by = 15),seq(from = 8, to = nrow(data),
    by = 15), seq(from = 9, to = nrow(data),by = 15),seq(from = 14, to = nrow(data),
    by = 15),seq(from = 15, to = nrow(data),by = 15)), 4] <- "Efficiency (gross, LHV)"
    
    data["technology"] <- "Nuclear"
    
    data["scenario"] <-  NA
    
    for (i in 1:9) {
      data[c(seq(from = i, to = nrow(data), by = 15)), 6] <- "Stated Policies"
    }
    data[which(is.na(data[,6])), 6] <- "Net Zero Emissions by 2050"
    
    data[["value"]] <- as.numeric(data[["value"]])
    
    # Convert to magpie object
    x <- as.quitte(data) %>% as.magpie()
  }
  
  if (subtype == "Fossil fuels equipped with CCUS") {
    # Read the Excel file
    data <- read_excel("WEO_2023_PG_Assumptions_STEPSandNZE_Scenario.xlsx", sheet = "Fossil fuels equipped with CCUS")
    data <- data[-c(1 : 6, 8),]
    data <- data[,-c(5, 9, 13, 16, 19)]
    names(data) <- data[1,]
    names(data)[1] <- "region"
    data <- data[-c(1, 11, 21 , 31),]
    
    data <- data %>% pivot_longer(!region, names_to = "period", values_to = "value")
    
    data["variable"] <- NA
    data[c(seq(from = 1, to = nrow(data), by = 15),seq(from = 2, to = nrow(data),
    by = 15), seq(from = 3, to = nrow(data),by = 15),seq(from = 10, to = nrow(data),
    by = 15),seq(from = 11, to = nrow(data),by = 15)), 4] <- "Capital costs (USD/kW)"
    
    data[c(seq(from = 4, to = nrow(data), by = 15),seq(from = 5, to = nrow(data),
    by = 15), seq(from = 6, to = nrow(data),by = 15),seq(from = 12, to = nrow(data),
    by = 15),seq(from = 13, to = nrow(data),by = 15)), 4] <- "Annual O&M Costs (USD/kW)"
    
    data[c(seq(from = 7, to = nrow(data), by = 15),seq(from = 8, to = nrow(data),
    by = 15), seq(from = 9, to = nrow(data),by = 15),seq(from = 14, to = nrow(data),
    by = 15),seq(from = 15, to = nrow(data),by = 15)), 4] <- "Efficiency (gross, LHV)"
    
    data["technology"] <- NA
    data[1 : 135, 5] <- "Coal + CCS"
    data[136 : 270, 5] <- "Oxyfuel + CCS"
    data[271 : 405, 5] <- "IGCC + CCS"
    data[406 : 540, 5] <- "CCGT + CCS"
    
    data["scenario"] <-  NA
    
    for (i in 1:9) {
      data[c(seq(from = i, to = nrow(data), by = 15)), 6] <- "Stated Policies"
    }
    data[which(is.na(data[,6])), 6] <- "Net Zero Emissions by 2050"
    
    data[["value"]] <- as.numeric(data[["value"]])
    
    # Convert to magpie object
    x <- as.quitte(data) %>% as.magpie()
  }
  
  if (subtype == "Coal") {
    # Read the Excel file
    data <- read_excel("WEO_2023_PG_Assumptions_STEPSandNZE_Scenario.xlsx", sheet = "Coal")
    data <- data[-c(1 : 6, 8),]
    data <- data[,-c(5, 9, 13, 16, 19)]
    names(data) <- data[1,]
    names(data)[1] <- "region"
    data <- data[-c(1, 11, 21 , 31),]
    
    data <- data %>% pivot_longer(!region, names_to = "period", values_to = "value")
    
    data["variable"] <- NA
    data[c(seq(from = 1, to = nrow(data), by = 15),seq(from = 2, to = nrow(data),
    by = 15), seq(from = 3, to = nrow(data),by = 15),seq(from = 10, to = nrow(data),
    by = 15),seq(from = 11, to = nrow(data),by = 15)), 4] <- "Capital costs (USD/kW)"
    
    data[c(seq(from = 4, to = nrow(data), by = 15),seq(from = 5, to = nrow(data),
    by = 15), seq(from = 6, to = nrow(data),by = 15),seq(from = 12, to = nrow(data),
    by = 15),seq(from = 13, to = nrow(data),by = 15)), 4] <- "Annual O&M Costs (USD/kW)"
    
    data[c(seq(from = 7, to = nrow(data), by = 15),seq(from = 8, to = nrow(data),
    by = 15), seq(from = 9, to = nrow(data),by = 15),seq(from = 14, to = nrow(data),
    by = 15),seq(from = 15, to = nrow(data),by = 15)), 4] <- "Efficiency (gross, LHV)"
    
    data["technology"] <- NA
    data[1 : 135, 5] <- "Steam Coal - SUBCRITICAL"
    data[136 : 270, 5] <- "Steam Coal - SUPERCRITICAL"
    data[271 : 405, 5] <- "Steam Coal - ULTRASUPERCRITICAL"
    data[406 : 540, 5] <- "IGCC"
    
    data["scenario"] <-  NA
    
    for (i in 1:9) {
      data[c(seq(from = i, to = nrow(data), by = 15)), 6] <- "Stated Policies"
    }
    data[which(is.na(data[,6])), 6] <- "Net Zero Emissions by 2050"
    
    data[["value"]] <- as.numeric(data[["value"]])
    
    # Convert to magpie object
    x <- as.quitte(data) %>% as.magpie()
  }
  
  if (subtype == "Gas") {
    # Read the Excel file
    data <- read_excel("WEO_2023_PG_Assumptions_STEPSandNZE_Scenario.xlsx", sheet = "Gas")
    data <- data[-c(1 : 6, 8),]
    data <- data[,-c(5, 9, 13, 16, 19)]
    names(data) <- data[1,]
    names(data)[1] <- "region"
    data <- data[-c(1, 11, 21 , 31),]
    
    data <- data %>% pivot_longer(!region, names_to = "period", values_to = "value")
    
    data["variable"] <- NA
    data[c(seq(from = 1, to = nrow(data), by = 15),seq(from = 2, to = nrow(data),
    by = 15), seq(from = 3, to = nrow(data),by = 15),seq(from = 10, to = nrow(data),
    by = 15),seq(from = 11, to = nrow(data),by = 15)), 4] <- "Capital costs (USD/kW)"
    
    data[c(seq(from = 4, to = nrow(data), by = 15),seq(from = 5, to = nrow(data),
    by = 15), seq(from = 6, to = nrow(data),by = 15),seq(from = 12, to = nrow(data),
    by = 15),seq(from = 13, to = nrow(data),by = 15)), 4] <- "Annual O&M Costs (USD/kW)"
    
    data[c(seq(from = 7, to = nrow(data), by = 15),seq(from = 8, to = nrow(data),
    by = 15), seq(from = 9, to = nrow(data),by = 15),seq(from = 14, to = nrow(data),
    by = 15),seq(from = 15, to = nrow(data),by = 15)), 4] <- "Efficiency (gross, LHV)"
    
    data["technology"] <- NA
    data[1 : 135, 5] <- "CCGT"
    data[136 : 270, 5] <- "Gas turbine"
    data[271 : 405, 5] <- "CCGT - CHP"
    data[406 : 540, 5] <- "Fuel cell (distributed electricity generation)"
    
    data["scenario"] <-  NA
    
    for (i in 1:9) {
      data[c(seq(from = i, to = nrow(data), by = 15)), 6] <- "Stated Policies"
    }
    data[which(is.na(data[,6])), 6] <- "Net Zero Emissions by 2050"
    
    data[["value"]] <- as.numeric(data[["value"]])
    
    # Convert to magpie object
    x <- as.quitte(data) %>% as.magpie()
  }
  
  list(x = x,
       weight = NULL,
       description = c(category = "Costs",
                       type = "Read PG_Assumptions TechCosts from the IEA World Energy Outlook 2023",
                       filename = "WEO_2023_PG_Assumptions_STEPSandNZE_Scenario.xlsx",
                       `Indicative size (MB)` = 0.012,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "E3M"))
}
