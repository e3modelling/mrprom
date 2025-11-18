#' calcIEnvPolicies
#'
#' Use carbon price data from the EU Reference Scenario 2020, as well as the
#' ENGAGE and NAVIGATE projects(carbon price data from the scenarios
#' GP_CurPol_T45, SUP_1p5C_Default and SUP_2C_Default) to derive OPENPROM input parameter iEnvPolicies.
#'
#' @return  OPENPROM input data iEnvPolicies.
#' The output data when overlapping between EU Reference Scenario 2020 and
#' the ENGAGE project takes the EU Reference Scenario 2020 value.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IEnvPolicies", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr left_join %>% filter select mutate
#' @importFrom tidyr expand_grid
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIEnvPolicies <- function() {
  
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  # Read in data from CarbonPrice_fromReportFig8.
  # The dataset contains carbon price data for the EU Reference Scenario 2020.
  a1 <- readSource("EU_RefScen2020")
  a1 <- a1 * 1.1 #from EUR15/tCO2 to US$2015/tCO2
  
  #The dataset contains carbon price data for the current policies scenario GP_CurPol_T45.
  a2 <- readSource("ENGAGE")
  a2 <- a2 * 1.087 #from US$2010/tCO2 to US$2015/tCO2

  q1 <- as.quitte(a1) %>%
    interpolate_missing_periods(period = getYears(a1, as.integer = TRUE), expand.values = FALSE)
  q2 <- as.quitte(a2)%>%
    interpolate_missing_periods(period = getYears(a2, as.integer = TRUE), expand.values = FALSE)
  
  q1["unit"] <- "US$2015/tCO2"
  q2["unit"] <- "US$2015/tCO2"
  value <- NULL
  q2 <- mutate(q2, value = mean(value, na.rm = TRUE), .by = c("region", "scenario", "unit", "period", "variable"))
  q2["model"] <- "Average of scenario GP_CurPol_T45"
  q2 <- q2 %>% distinct()

  qx <- left_join(q2, q1, by = c("region", "period", "variable", "unit"))

  # The output data when overlapping between EU Reference Scenario 2020 and
  # the ENGAGE project takes the EU Reference Scenario 2020 value.
  qx[["value.x"]] <- ifelse(!is.na(qx[["value.y"]]) & (qx[["value.y"]]) > 0, qx[["value.y"]], qx[["value.x"]])
  qx[["model.x"]] <- ifelse(!is.na(qx[["value.y"]]) & (qx[["value.y"]]) > 0, as.character(qx[["model.y"]]), as.character(qx[["model.x"]]))
  qx[["scenario.x"]] <- ifelse(!is.na(qx[["value.y"]]) & (qx[["value.y"]]) > 0, as.character(qx[["scenario.y"]]), as.character(qx[["scenario.x"]]))

  qx <- select(qx, -c("model.y", "scenario.y", "value.y"))
  names(qx)[1] <- "model"
  names(qx)[2] <- "scenario"
  names(qx)[7] <- "value"

  qx["scenario"] <- NA
  qx["model"] <- NA
  qx["unit"] <- NA

  qx <- as.quitte(qx)

  qx <- interpolate_missing_periods(qx, 2010:2100, expand.values = TRUE)
  period <- NULL
  qx <- filter(qx, period >= 2010)
  
  ## Wolrd Bank Carbon Price until 2024
  WB <- readSource("WorldBankCarPr")
  WB <- as.quitte(WB)
  
  # Wolrd Bank and ENGAGE and EU Reference Scenario 2020
  # The output data when overlapping between Wolrd Bank, EU Reference Scenario 2020 and
  # the ENGAGE project takes the Wolrd Bank value.
  WB[["unit"]] <- NA
  WB[["model"]] <- NA
  WB[["scenario"]] <- NA
  qx <- full_join(WB, qx, by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
    mutate(value = ifelse(is.na(value.x) | value.x == 0, value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  
  # Loading the REMIND 1.5C and 2C scenario carbon prices
  q3 <- readSource("Navigate", subtype = "SUP_1p5C_Default", convert = TRUE)
  q3 <- q3[,,"REMIND-MAgPIE 3_2-4_6.SUP_1p5C_Default.Price|Carbon.US$2010/t CO2"]
  q3 <- as.quitte(q3)
  q3 <- interpolate_missing_periods(q3, 2010:2100, expand.values = TRUE)
  q3 <- as.magpie(q3)
  q3[,,"REMIND-MAgPIE 3_2-4_6.SUP_1p5C_Default.Price|Carbon.US$2010/t CO2"] <- q3["JPN",,"REMIND-MAgPIE 3_2-4_6.SUP_1p5C_Default.Price|Carbon.US$2010/t CO2"]
  
  q4 <- readSource("Navigate", subtype = "SUP_2C_Default", convert = TRUE)
  q4 <- q4[,,"REMIND-MAgPIE 3_2-4_6.SUP_2C_Default.Price|Carbon.US$2010/t CO2"]
  q4 <- as.quitte(q4)
  q4 <- interpolate_missing_periods(q4, 2010:2100, expand.values = TRUE)
  q4 <- as.magpie(q4)
  q4[,,"REMIND-MAgPIE 3_2-4_6.SUP_2C_Default.Price|Carbon.US$2010/t CO2"] <- q4["JPN",,"REMIND-MAgPIE 3_2-4_6.SUP_2C_Default.Price|Carbon.US$2010/t CO2"]
  
  
  # load current OPENPROM set configuration
  sets <- toolGetMapping(name = "POLICIES_set.csv",
                         type = "blabla_export",
                         where = "mrprom")
  
  POLICIES_set <- as.character(sets[c(sets[, 1] %in% c("TRADE", "OPT", "REN", "EFF", "exogCV_NPi", "exogCV_1_5C", "exogCV_2C")), 1])

  x <- expand_grid(POLICIES_set, unique(qx["region"]), unique(qx["period"]))
  x["value"] <- NA
  #the variable is exogCV
  #the variables TRADE, OPT, REN, EFF are NA
  x[x["POLICIES_set"] == "exogCV_NPi", 4] <- qx["value"]
  
  # Converting quitte to magpie 
  x <- as.quitte(x) %>% as.magpie()
  
  # Getting the carbon price values from REMIND scenarios (converting US$2010 to US$2015) 
  x[, , "exogCV_1_5C"] <- q3[,,"REMIND-MAgPIE 3_2-4_6.SUP_1p5C_Default.Price|Carbon.US$2010/t CO2"] * 1.087
  x[, , "exogCV_2C"] <- q4[,,"REMIND-MAgPIE 3_2-4_6.SUP_2C_Default.Price|Carbon.US$2010/t CO2"] * 1.087
  
  a1 <- readSource("EU_RefScen2020")
  a1 <- a1 * 1.1 #from EUR15/tCO2 to US$2015/tCO2
  
  qa1 <- as.quitte(a1) %>%
    interpolate_missing_periods(period = fStartHorizon:2100, expand.values = TRUE)
  
  qa1 <- filter(qa1, value >0)
  
  qa1 <- as.quitte(qa1) %>% as.magpie()
  
  qa1 <- collapseDim(qa1,3)
  
  a3 <- readSource("WEO2023CarbonPrices")
  
  qa3 <- as.quitte(a3) %>%
    interpolate_missing_periods(period = fStartHorizon:2100, expand.values = TRUE)
  
  qa3 <- as.quitte(qa3) %>% as.magpie()
  
  qa3 <- collapseDim(qa3,3)
  
  qcalib <- mbind(qa1, qa3)
  
  getItems(qcalib,3) <- "exogCV_Calib"
  
  qcalib <- toolCountryFill(qcalib, fill = 0)

  x <- mbind(x, qcalib)
  
  list(x = x,
       weight = NULL,
       unit = "various",
       description = "Carbon price data from EU Reference Scenario 2020, ENGAGE, NAVIGATE projects")

}
