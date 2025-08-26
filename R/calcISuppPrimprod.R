#' calcISuppPrimprod
#'
#' Use ENERDATA Primary production data to derive OPENPROM input parameter iSuppPrimProd.
#'
#' @return  OPENPROM input data iSuppPrimProd.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "ISuppPrimprod", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom tidyr pivot_wider separate_rows
#' @importFrom quitte as.quitte


calcISuppPrimprod <- function() {

  d <- readSource("IEA2025", subtype = "INDPROD")
  d <- d[,,"KTOE"]
  getItems(d,3.1) <- "Mtoe"
  d <- d / 1000 #ktoe to mtoe
  
  d <- d[,2010:2023,]
  
  df <- data.frame(
    variable = c("HCL", "LGN","CRO","HYD","BMSWAS","NUC","SOL","GEO","WND","NGS"),
    OP = c("ANTHRACITE,COKING_COAL,OTH_BITCOAL","BKB,LIGNITE,SUB_BITCOAL","CRUDE_OIL",
           "HYDRO","PRIMARY_SOLID_BIOFUEL,BIOGASES","NUCLEAR","SOLAR_PV,SOLAR_THERMAL",
           "GEOTHERMAL","WIND","NATURAL_GAS"))
  
  df <- separate_rows(df, OP, sep = ",")
  
  d <- d[,,c(df[["OP"]])]
  
  d <- collapseDim(d,3.3)
  
  d[is.na(d)] <- 0
  
  d <- toolAggregate(d,dim = 3.2,rel = df,from = "variable",to = "OP")
  
  qx <- as.quitte(d)
  
  qx <- select(qx, -variable)
  
  names(qx) <- sub("product", "variable", names(qx))
  
  qx <- as.quitte(qx)
  
  # complete incomplete time series
  qx <- qx %>%
    interpolate_missing_periods(period = getYears(d, as.integer = TRUE), expand.values = TRUE)
  
  qx_cro <- qx[which(qx[,4] == "CRO"), ]  %>%
    interpolate_missing_periods(period = 2010 : 2100, expand.values = TRUE)
  
  qx <- rbind(qx, qx_cro[which(qx_cro[,6] > 2023), ])
  
  # assign to countries with NA, their H12 region with weights
  # h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  # 
  # qx <- select(qx, -c("model", "scenario"))
  # qx_bu <- qx
  # 
  # ## assign to countries with NA, their H12 region with weights calculated from population
  # 
  # population <- calcOutput(type = "POP", aggregate = FALSE)
  # population <- as.quitte(population)
  # 
  # # compute weights by population
  # names(population) <- sub("region", "CountryCode", names(population))
  # 
  # ## add mapping to population
  # population <- left_join(population, h12, by = "CountryCode")
  # value.x <- NULL
  # value.y <- NULL
  # weights <- NULL
  # value <- NULL
  # POP <- mutate(population, weights = sum(value, na.rm = TRUE), .by = c("RegionCode", "period"))
  # POP["weights"] <- POP["value"] / POP["weights"]
  # 
  # names(POP) <- sub("CountryCode", "region", names(POP))
  # POP <- select(POP, -c("value", "model", "scenario", "X", "variable", "unit"))
  # qx <- left_join(qx, POP, by = c("region", "period"))
  # 
  # qx <- mutate(qx, value = sum(value, na.rm = TRUE), .by = c("RegionCode", "period", "variable", "unit"))
  # 
  # qx["value"] <- qx["value"] * qx["weights"]
  # 
  # qx <- select(qx, -c("weights"))
  # 
  # qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
  #   mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
  #   select(-c("value.x", "value.y", "RegionCode"))
  # 
  # ## assign to countries that still have NA, the global with weights
  # qx_bu <- qx
  # # compute weights by population
  # POP <- mutate(population, weights = sum(value, na.rm = TRUE), .by = c("period"))
  # POP["weights"] <- POP["value"] / POP["weights"]
  # names(POP) <- sub("CountryCode", "region", names(POP))
  # POP <- select(POP, -c("value", "model", "scenario", "X", "RegionCode", "variable", "unit"))
  # qx <- left_join(qx, POP, by = c("region", "period"))
  # 
  # qx <- mutate(qx, value = sum(value, na.rm = TRUE), .by = c("period", "variable", "unit"))
  # 
  # qx["value"] <- qx["value"] * qx["weights"]
  # 
  # qx <- select(qx, -c("weights"))
  # 
  # qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
  #   mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
  #   select(-c("value.x", "value.y"))
  x <- as.quitte(qx) %>% as.magpie()
  # set NA to 0
  x[is.na(x)] <- 0

  list(x = collapseNames(x),
       weight = NULL,
       unit = getItems(x, 3.2)[1],
       description = "Enerdata; Primary production")

}
