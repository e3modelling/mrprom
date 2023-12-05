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

  x <- readSource("ENERDATA", "distr", convert = TRUE)
  
  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  x <- x[, c(max(fStartHorizon, min(getYears(x, as.integer = TRUE))) : max(getYears(x, as.integer = TRUE))), ]
  
  # Use ENERDATA - OPENPROM mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-enerdata-distrloss-mapping.csv",
                        type = "sectoral",
                        where = "mappingfolder")
  
  ## Only keep items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA..Mtoe."]), "ENERDATA..Mtoe."])
  map <- map[map[, "ENERDATA..Mtoe."] %in% enernames, ]

  enernames <- unique(map[!is.na(map[, "ENERDATA..Mtoe."]), "ENERDATA..Mtoe."])
  enernames <- enernames[! enernames %in% c("")]

  x <- x[, , enernames]
  x <- x[, , "Mtoe", pmatch = TRUE]

  ## Rename variables from ENERDATA to OPENPROM names
  ff <- map[!(map[, 2] == ""), 1]
  getNames(x) <- ff
  
  # Adding the STE variable with (nearly) zero values as in MENA-EDS
  x <- add_columns(x, addnm = "STE", dim = "variable", fill = 0.00000001)

  # Converting to quitte object and interpolating periods
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = getYears(x, as.integer = TRUE), expand.values = TRUE)
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
       description = "Enerdata; Distribution Losses")
}
