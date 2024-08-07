#' calcIElcNetImpShare
#'
#' Use data from ENERDATA to derive OPENPROM input parameter iElcNetImpShare.
#' This dataset includes the ratio of electricity imports in total final demand.
#'
#' @return magpie object with OPENPROM input data iSuppRefCapacity.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IElcNetImpShare", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join case_when if_else arrange
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom tibble deframe
#' @importFrom utils tail

calcIElcNetImpShare <- function() {
  
  x <- readSource("ENERDATA", "Electricity", convert = TRUE)
  
  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  lastYear <- tail(sort(getYears(x, as.integer = TRUE)), 1)
  x <- x[, c(fStartHorizon:lastYear), ]
  
  ## Only keep items with the Mtoe unit
  x <- x[, , "Mtoe", pmatch = TRUE]
  
  # Adding the PROM variable with placeholder values
  promnames <- "ELC_IMP"
  
  x <- add_columns(x, addnm = "ELC_IMP", dim = "variable", fill = 0.00000001)
  
  # Assigning the variables that require calculations
  x[, , "ELC_IMP"] <- (x[, , "Imports of electricity"] - x[, , "Exports of electricity"]) / x[, , "Electricity final consumption"]

  # Only keeping the PROM variable and dropping the rest    
  x <- x[, , promnames]
  
  # Converting to quitte object and interpolating periods
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = c(fStartHorizon : 2100), expand.values = TRUE)
  
  # for ELC_IMP.Mtoe after 2021 each year is the previous year multiplied by 0.98
  for (i in  (lastYear + 1): 2100) {
    
    qx[which(qx["period"] == i), 7] <-  qx[which(qx["period"] == (i - 1)), 7] * 0.98
    
  }
  
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
  x <- as.quitte(qx) %>% as.magpie()
  # Set NA to 0
  x[is.na(x)] <- 0
  list(x = x,
       weight = NULL,
       unit = "Mtoe",
       description = "Enerdata; Refineries capacity and ratio of electricity imports")
}
