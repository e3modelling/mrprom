#' calcIEnvPolicies
#'
#' Use carbon price data from the EU Reference Scenario 2020 and from the
#' ENGAGE project (carbon price data for the current policies scenario
#' GP_CurPol_T45) to derive OPENPROM input parameter iEnvPolicies.
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
  
  # Read in data from CarbonPrice_fromReportFig8.
  # The dataset contains carbon price data for the EU Reference Scenario 2020.
  a1 <- readSource("EU_RefScen2020")
  a1 <- a1 / 1.1792 #from EUR15/tCO2 to EUR05/tCO2
  
  #The dataset contains carbon price data for the current policies scenario GP_CurPol_T45.
  a2 <- readSource("ENGAGE")
  a2 <- a2 * 0.8257299 #from US$2010/tCO2 to EUR05/tCO2

  q1 <- as.quitte(a1) %>%
    interpolate_missing_periods(period = getYears(a2, as.integer = TRUE), expand.values = FALSE)
  q2 <- as.quitte(a2)
  q1["unit"] <- "EUR05/tCO2"
  q2["unit"] <- "EUR05/tCO2"
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


  # load current OPENPROM set configuration
  POLICIES_set <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "POLICIES_set")
  POLICIES_set <- unlist(strsplit(POLICIES_set[, 1], ","))
  POLICIES_set <- POLICIES_set[c(1, 3:5, 11)]

  x <- expand_grid(POLICIES_set, unique(qx["region"]), unique(qx["period"]))
  x["value"] <- NA
  #the variable is exogCV
  #the variables TRADE, OPT, REN, EFF are NA
  x[x["POLICIES_set"] == "exogCV", 4] <- qx["value"]

  x <- as.quitte(x) %>% as.magpie()

  list(x = x,
       weight = NULL,
       unit = "various",
       description = "Carbon price data from EU Reference Scenario 2020 and policies scenario GP_CurPol_T45")

}
