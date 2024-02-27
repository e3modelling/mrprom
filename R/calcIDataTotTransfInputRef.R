#' calcIDataTotTransfInputRef
#'
#' Use data from ENERDATA to derive OPENPROM input parameter iDataTotTransfInputRef.
#' This dataset has the refineries input in Mtoe.
#'
#' @return  OPENPROM input data iDataTotTransfInputRef.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataTotTransfInputRef", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select filter mutate
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIDataTotTransfInputRef <- function() {
  
  a <- readSource("ENERDATA", "input", convert = TRUE)
  b <- a[, , "Crude oil consumption of refineries input"]
  y <- a[, , "NGL (natural gas liquids) refineries input"]
  
  x <- mbind(b, y)
  x <- x[, , "Mtoe"]
  x <- dimSums(x, dim = 3, na.rm = TRUE)
  x <- setNames(x, "CRO.Mtoe")
  getSets(x)[3] <- "variable.unit"
  
  # Adding the PROM variables with placeholder values
  promnames <- c("LGN", "HCL")
  for (name in promnames) {
    x <- add_columns(x, addnm = name, dim = "variable", fill = 0.00000001)
  }
  
  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fEndHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndHorizon"]
  
  x <- x[, fStartHorizon : tail(getYears(x, as.integer = TRUE), n = 1), ]
  
  # complete incomplete time series
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = fStartHorizon : fEndHorizon, expand.values = TRUE)
  qx_bu <- qx
  # assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  names(qx) <- sub("region", "CountryCode", names(qx))
  ## add h12 mapping to dataset
  qx <- left_join(qx, h12, by = "CountryCode")
  ## add new column containing regional mean value
  value <- NULL
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "variable"))
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
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "variable"))
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  x <- as.quitte(qx) %>% as.magpie()
  # set NA to 0
  x[is.na(x)] <- 0
  
  return(list(x = x,
              weight = NULL,
              unit = getItems(x, 3.2),
              description = "Enerdata; refineries input"))
  
}
