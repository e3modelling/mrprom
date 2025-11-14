#' calcIInpTransfTherm
#'
#' Use ENERDATA input in electricity power plants data to derive OPENPROM input
#' parameter iInpTransfTherm
#'
#' @return  OPENPROM input data iInpTransfTherm
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IInpTransfTherm", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join if_else
#' @importFrom quitte as.quitte interpolate_missing_periods


calcIInpTransfTherm <- function() {
  
  # load data source (ENERDATA)
  x <- readSource("ENERDATA", "input", convert = TRUE)
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  x <- x[, c(max(fStartHorizon, min(getYears(x, as.integer = TRUE))) : max(getYears(x, as.integer = TRUE))), ]
  
  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-enerdata-input-in-electricity-power-plant-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")

  ## ..and items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  map <- map[map[, "ENERDATA"] %in% enernames, ]
  ## filter data
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  x <- x[, , enernames]
  
  # rename variables to openprom names
  getItems(x, 3.1) <- map[map[["ENERDATA"]] %in% getItems(x, 3), "EFS"]
  
  # Mt to Mtoe
  # t = 0,7 toe (coal)
  # t = 0,48 toe (lignite)
  x[,,"HCL.Mt"] <- 0.7 * x[,,"HCL.Mt"]
  x[,,"LGN.Mt"] <- 0.48 * x[,,"LGN.Mt"]
  
  getItems(x, 3.2) <- c("Mtoe", "Mtoe")
  
  # complete incomplete time series
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = 2010:2024, expand.values = TRUE)
  
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
  
  list(x = collapseNames(x),
       weight = NULL,
       unit = getItems(x, 3.2)[1],
       description = "Enerdata; input in electricity power plants")
  
}
