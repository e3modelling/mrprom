#' readCarPrSoCDRHighestAmbition
#'
#' Read CarPrSoCDRHighestAmbition.
#' The dataset contains carbon price data.
#'
#' @return The read-in carbon price data into a magpie object
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("CarPrSoCDRHighestAmbition")
#' }
#'
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom utils read.csv
#' @importFrom quitte as.quitte
#'
readCarPrSoCDRHighestAmbition <- function() {
  fStartHorizon <- toolReadEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fEndHorizon <- toolReadEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndHorizon"]
  x <- read.csv(file = "CarPrSoCDRHighestAmbitionFirst.csv")

  names(x) <- sub("X", "", names(x))

  x <- x %>%
    pivot_longer(
      cols = `2010`:`2100`,
      names_to = "period",
      values_to = "value"
    )

  x <- as.quitte(x) %>% as.magpie()

  map <- toolGetMapping("regionmappingOPDEV5.csv", "regional", where = "mrprom")

  SoCDRHighestAmbition <- toolAggregate(x, rel = map, weight = NULL, from = "Region.Code", to = "ISO3.Code", dim = 1)
  
  ## Wolrd Bank Carbon Price until 2024
  
  WB <- readSource("WorldBankCarPr", convert = FALSE)
  
  map <- toolGetMapping(name = "EU28.csv",
                        type = "regional",
                        where = "mrprom")
  
  
  # Take EU for for 28 EU countries
  map[["EU"]] <- "EU"
  
  EU_wb_car_pr <- toolAggregate(WB["EU",,], dim = 1, rel = map, from = "EU", to = "ISO3.Code")
  
  WB <- full_join(as.quitte(EU_wb_car_pr), as.quitte(WB), by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  
  WB <- as.quitte(WB) %>% as.magpie()
  
  WB <- toolCountryFill(WB, fill = NA)
  
  WB <- as.quitte(WB)
  
  WB[["unit"]] <- "(Missing)"
  WB[["model"]] <- "(Missing)"
  WB[["scenario"]] <- "(Missing)"
  
  SoCDRHighestAmbition <- as.quitte(SoCDRHighestAmbition)
  SoCDRHighestAmbition[["variable"]] <- "Price|Carbon"
  WB[["policy"]] <- "exogCV_1_5C"
  
  # if 0 put NA, we do it because in some countries we have data after 2020.
  WB$value[WB$value == 0] <- NA
  WB <- as.quitte(WB) %>% 
    interpolate_missing_periods(period = fStartHorizon : 2024, expand.values = TRUE)
  
  qx <- full_join(WB, SoCDRHighestAmbition, by = c("model", "scenario", "region", "period", "variable", "unit", "policy")) %>%
    mutate(value = ifelse(is.na(value.x) | value.x == 0, value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  
  qx <- fix_values(qx)
  qx <- select(qx, -c( "value" ))
  names(qx) <- sub("value_fixed","value",names(qx))
  
  interpolate <- as.quitte(qx) %>% as.magpie()
  interpolate[,2025:2030,] <- NA
  
  interpolate <- as.quitte(interpolate) %>% 
    interpolate_missing_periods(period = fStartHorizon : fEndHorizon, expand.values = TRUE)
  
  x <- as.quitte(interpolate) %>% as.magpie()

  list(x = x,
       weight = NULL,
       description = c(category = "Costs",
                       type = "Carbon Price",
                       filename = "CarPrSoCDRHighestAmbition.csv",
                       `Indicative size (MB)` = 0.21,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "project"))

}


# Helper ------------------------------------------------
fix_values <- function(df) {
  df %>% 
    group_by(region, variable) %>%      # per region (and variable if needed)
    arrange(period, .by_group = TRUE) %>%
    mutate(
      value_fixed = cummax(value)       # keep increases, hold the higher if decreases
    ) %>%
    ungroup()
}