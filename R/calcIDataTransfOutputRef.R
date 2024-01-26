#' calcIDataTransfOutputRef
#'
#' Use data from ENERDATA to derive OPENPROM input parameter iDataTransfOutputRef.
#' This dataset includes the transformation Output from Refineries.
#' 
#' @return magpie object with OPENPROM input data iDataImports. 
#' 
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataTransfOutputRef", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join if_else
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIDataTransfOutputRef <- function() {
  
  x <- readSource("ENERDATA", "production", convert = TRUE)
  
  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  lastYear <- sub("y", "", tail( sort(getYears(x)), 1))
  x <- x[, c(fStartHorizon:lastYear), ]
  
  # Use ENERDATA - OPENPROM mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-enerdata-refineries-mapping.csv",
                        type = "sectoral",
                        where = "mappingfolder")
  
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  
  ## Only keep items that have an enerdata-prom mapping
  x <- x[, , enernames]
  
  # Adding the PROM variables with placeholder values
  x <- add_columns(x, addnm = "OLQ.Mtoe", dim = 3, fill = 0.00000001)
  
  getItems(x, 3.1) <- map[["EF"]]
  
  # Converting to quitte object and interpolating periods
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = 2010:2100, expand.values = TRUE)
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
       unit = "Mtoe or Mt",
       description = "Enerdata; Transformation Output Refineries")
}
