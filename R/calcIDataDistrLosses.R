#' calcIDataDistrLosses
#'
#' Use data to derive OPENPROM input parameter iDataDistrLosses
#'
#' @return  OPENPROM input data iDataDistrLosses
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataDistrLosses", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join case_when if_else arrange
#' @importFrom tidyr pivot_wider spread gather
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom utils tail

calcIDataDistrLosses <- function() {

  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  x <- readSource("IEA2025", subset = "DISTLOSS")
  x <- x[,,"KTOE"]
  getItems(x,3.1) <- "Mtoe"
  x <- x / 1000 #ktoe to mtoe

  x <- x[, c(max(fStartHorizon, min(getYears(x, as.integer = TRUE))) : max(getYears(x, as.integer = TRUE))), ]

  # Use ENERDATA - OPENPROM mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-IEA-distrloss-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")
  
  x <- x[,,map[,"fuel"]]
  
  x[is.na(x)] <- - 1e-06
  
  x <- toolAggregate(x, dim = 3.2, rel = map, from = "fuel", to = "OPEN.PROM")
  x <- x * (-1)
  
  x <- collapseDim(x, 3.3)

  # Converting to quitte object and interpolating periods
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = getYears(x, as.integer = TRUE), expand.values = TRUE)
  
  qx[["variable"]] <- qx[["product"]]
  
  qx<- select(qx, - product)
  
  # qx_bu <- qx
  # 
  # # Assign to countries with NA, their H12 region mean
  # h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  # names(qx) <- sub("region", "CountryCode", names(qx))
  # 
  # ## Add h12 mapping to dataset
  # qx <- left_join(qx, h12, by = "CountryCode")
  # 
  # ## Add new column containing regional mean value
  # value <- NULL
  # qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "variable"))
  # names(qx) <- sub("CountryCode", "region", names(qx))
  # qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  # qx_bu <- select(qx_bu, -c("model", "scenario"))
  # 
  # ## Assign the H12 region mean where necessary
  # value.x <- NULL
  # value.y <- NULL
  # qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
  #   mutate(value = ifelse(value.x == 1e-06, value.y, value.x)) %>%
  #   select(-c("value.x", "value.y"))
  # 
  # ## Assign to countries that still have NA, the global mean
  # qx_bu <- qx
  # qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "variable"))
  # qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
  #   mutate(value = ifelse(value.x == 1e-06, value.y, value.x)) %>%
  #   select(-c("value.x", "value.y"))

  # Converting to magpie object
  x <- as.quitte(qx) %>% as.magpie()

  # Set NA to 0
  x[is.na(x)] <- 10^-6
  list(x = x,
       weight = NULL,
       unit = "Mtoe",
       description = "Enerdata; Distribution Losses")
}
