#' calcIDataPassCars
#'
#' Use data to derive OPENPROM input parameter IDataPassCars
#'
#' @return  OPENPROM input data iDataPassCars
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataPassCars", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte

calcIDataPassCars <- function() {
  
  x <- readSource("Eurostat_ELVS", convert =TRUE)
  
  y <- as.quitte(x) %>% as.magpie()
  
  a1 <- readSource("IRF", subtype = "passenger-cars-in-use")
  a2 <- readSource("IRF", subtype = "total-vehicles-in-use")
  
  a1 <- a1[,Reduce(intersect, list(getYears(a1),getYears(a2))),]
  a2 <- a2[,Reduce(intersect, list(getYears(a1),getYears(a2))),]
  
  a <- a1/a2
  a <- a/a2
  
  a <- a[,Reduce(intersect, list(getYears(a),getYears(y))),]
  y <- y[,Reduce(intersect, list(getYears(a),getYears(y))),]
  
  x <- a*y
  
  getNames(x) <- "PC"
  getSets(x) <- c("region", "period", "unit")
  
  y <- readSource("BoT")
  
  
  getNames(y) <- "PC"
  getSets(y) <- c("region", "period", "unit")
  
  a <- a1/a2
  a <- a*(1/a2)
  a <- a["USA", , ]
  
  y <- as.quitte(y) %>%
    interpolate_missing_periods(period = getYears(a, as.integer = TRUE), expand.values = TRUE)  
  
  y <- as.quitte(y) %>% as.magpie()
  
  a <- a[,Reduce(intersect, list(getYears(a),getYears(y))),]
  y <- y[,Reduce(intersect, list(getYears(a),getYears(y))),]
  
  k <- a*y
  
  x["USA",,] <- k
  
  qx <- as.quitte(x)
  qx_bu <- qx
  # assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv")
  names(qx) <- sub("region", "CountryCode", names(qx))
  ## add h12 mapping to dataset
  qx <- left_join(qx, h12, by="CountryCode")
  ## add new column containing regional mean value
  value <- NULL
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "unit", "variable"))
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
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "variable", "unit"))
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>% 
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>% 
    select(-c("value.x", "value.y"))

  x <- as.quitte(qx) %>% as.magpie()
  
  list(x = x,
       weight = NULL,
       unit = "reuse_pc",
       description = "reuse_pc")
}