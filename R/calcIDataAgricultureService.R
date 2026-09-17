#' calcIDataAgricultureService
#' Units : Crops - 1e9 ha, Livestock - 1e9 An, Forestry - 1e6 m3,
#'         Fishing - 1e6 tonnes, IRRIGATION - 1e9 ha
#'
#' @return  Magpie object with the FAOProductionCrops
#'
#' @author Fotis Sioutas
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataAgricultureService", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter left_join mutate select %>%
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom R.utils isZero

calcIDataAgricultureService <- function() {
  
  x <- readSource("FAOProductionCrops", convert = TRUE)
  x <- collapseDim(x, 3.2)
  x <- collapseDim(x, 3.1)
  x[is.na(x)] <- 0
  x <- x[,,c("ha","An","1000 An")]
  AreaHarvested <- x[,,c("Area harvested")]
  AreaHarvested <- AreaHarvested / 1000
  getItems(AreaHarvested, 3.2) <- "1000 ha"
  getItems(AreaHarvested, 3.1) <- "CROPS"
  Stocks <- x[,,c("1000 An")]
  Stocks2 <- x[,,c("An")]
  Stocks2 <- Stocks2 / 1000
  getItems(Stocks2, 3.2) <- "1000 An"
  Animal_stocks <- mbind(Stocks, Stocks2)
  getItems(Animal_stocks, 3.1) <- "LIVESTOCK"
  data <- mbind(AreaHarvested, Animal_stocks)
  data <- dimSums(data, 3.3)
  data <- data / 1000000
  getItems(data, 3.2) <- c("1e9 ha", "1e9 An")
  
  # complete incomplete time series
  qx <- as.quitte(data) %>%
    interpolate_missing_periods(period = getYears(data, as.integer = TRUE), expand.values = TRUE)
  
  x1 <- as.magpie(qx)
  
  FAOLandUse <- readSource("FAOLandUse")
  FAOLandUse <- FAOLandUse[,getYears(x1),]
  shareirrigated <- FAOLandUse[,,"Cropland area actually irrigated"][,,"Area"]/FAOLandUse[,,"Cropland"][,,"Area"]
  shareirrigated <- collapseDim(shareirrigated, 3)
  shareirrigated <- add_dimension(shareirrigated, dim = 3, add = "variable", nm = "shareirrigated")
  shareirrigated["EGY",,] <- NA
  
  # complete incomplete time series
  qx <- as.quitte(shareirrigated) %>%
    interpolate_missing_periods(period = getYears(shareirrigated, as.integer = TRUE), expand.values = TRUE)
  
  qx_bu <- qx
  
  # Assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  names(qx) <- sub("region", "CountryCode", names(qx))
  
  ## Add h12 mapping to dataset
  qx <- left_join(qx, h12, by = "CountryCode")
  
  ## Add new column containing regional mean value
  value <- NULL
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "variable"))
  names(qx) <- sub("CountryCode", "region", names(qx))
  qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  qx_bu <- select(qx_bu, -c("model", "scenario"))
  
  ## Assign the H12 region mean where necessary
  value.x <- NULL
  value.y <- NULL
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  
  ## Assign to countries that still have NA, the global mean
  qx_bu <- qx
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "variable"))
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  
  # Converting to magpie object
  x2 <- as.quitte(qx) %>% as.magpie()
  # Set NA to 0
  x2[is.na(x2)] <- 0
  joinx2 <- as.quitte(x2) %>% select(-c("model", "scenario", "unit", "variable"))
  
  x3 <- left_join(
    as.quitte(x1),
    joinx2,
    by = c("region", "period")
  ) %>%
    filter(variable == "CROPS") %>%
    mutate(
      value = value.x * value.y,
      variable = "IRRIGATION"
    ) %>%
    select(-c(value.x, value.y))
  
  x3 <- as.quitte(x3) %>% as.magpie()
  getItems(x3, 3.2) <- "1e9 ha"
  
  FAOForestry <- readSource("FAOForestry")
  Roundwood <- FAOForestry[,,"Roundwood"][,,"Production"]
  Roundwood[is.na(Roundwood)] <- 0
  Roundwood <- collapseDim(Roundwood, 3.3)
  getItems(Roundwood, 3.1) <- "FORESTRY"
  Roundwood <- Roundwood / 1000000
  getItems(Roundwood, 3.2) <- "1e6 m3"
  
  FAOFishing <- readSource("FAOFishing")
  FAOFishing[is.na(FAOFishing)] <- 0
  FAOFishing <- dimSums(FAOFishing, 3)
  getItems(FAOFishing, 3.1) <- "FISHING"
  FAOFishing <- FAOFishing / 1000000
  getItems(FAOFishing, 3.2) <- "1e6 tonnes"
  
  # ------------------------------------------------------------------
  # Climate
  data <- readSource("AGENRES")
  data <- data[,,"Greenhouses.ha.The area under high covers"]
  getItems(data, 2) <- "y2023"
  data <- collapseDim(data, 3.3)
  getItems(data, 3.1) <- "Climate"
  
  map <- toolGetMapping("AGENRES_249_to_7regions.csv", "regional", where = "mrprom")
  
  # ------- RaboResearch_Global-greenhouse-update_2025.pdf
  
  regions <- c("Europe","North America","South America","Asia (excluding China)",
               "China","Africa","Oceania")
  values <- c(188772,71155,32810,300594,2000000,68512,2509) 
  
  climate <- new.magpie(cells_and_regions = regions,
                        years = "y2023",
                        names = "CLIMATE",   fill = 0)
  
  climate[, "y2023", "CLIMATE"] <- values
  
  getSets(climate) <- c("region", "year", "variable")
  
  climate <- add_dimension(climate, dim = 3.2, add = "unit", nm = "ha")
  
  gdp <- calcOutput("iGDP", aggregate = FALSE)
  
  gdp <- gdp[,getYears(climate),]
  
  climate249 <- toolAggregate(climate,
                            dim = 1, weight = gdp,
                            rel = map, from = "Region.Code", to = "ISO3.Code")
  # GBR was 0 in readSource("AGENRES") so drop
  data <- data[setdiff(getRegions(data), "GBR"), , ]
  
  climate249[getRegions(data), , ] <- data
  climate249 <- climate249 / 1e6
  getItems(climate249, 3.2) <- "1e6 ha"
  
  climate249 <- as.quitte(climate249) %>%
    interpolate_missing_periods(period = getYears(x, as.integer = TRUE), expand.values = TRUE)  %>% as.magpie()
  
  x <- mbind(x1, x3, Roundwood, FAOFishing, climate249)
  

  # -------------- weights -------------------------------------------
  # ------------------------------------------------------------------
  # Calculation of aggregation weights
  
  Population <- calcOutput("POP", aggregate = FALSE)
  Population <- Population[, getYears(x), , drop = TRUE]
  
  weights <- x
  weights[, , ] <- Population
  weights[, , c("CROPS", "LIVESTOCK", "FORESTRY", "FISHING")] <- NA
  
  list(
    x = x,
    weight = weights,
    unit = "various",
    description = "FAOProductionCrops",
    mixed_aggregation = TRUE
  )
}
