#' calcISuppTransfers
#'
#' Use data from ENERDATA to derive OPENPROM input parameter iSuppTransfers
#' This dataset includes the supplementary parameter for transfers, in Mt.
#'
#' @return magpie object with OPENPROM input data iSuppTransfers.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "ISuppTransfers", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join case_when if_else arrange
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom tibble deframe
#' @importFrom utils tail

calcISuppTransfers <- function() {

  x <- readSource("ENERDATA", "Returns", convert = TRUE)

  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  lastYear <- tail(sort(getYears(x, as.integer = TRUE)), 1)
  x <- x[, c(fStartHorizon:lastYear), ]
  
  # Use ENERDATA - OPENPROM mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-enerdata-transfers-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")
  
  promnames <- map[["OPEN.PROM"]]
  enernames <- map[["ENERDATA"]]
  enernames <- enernames[nzchar(enernames)]
  map_kv <- deframe(map[1:2])
  
  # Only keep the relevant ENERDATA entries
  x <- x[, , enernames]

  # Adding the PROM variables with placeholder values
  for (name in promnames) {
    x <- add_columns(x, addnm = name, dim = "variable", fill = 0.00000001)
  }

  # Assigning the variables that map 1-to-1 between PROM and ENERDATA
  for (key in names(map_kv[nzchar(map_kv)])) {
    modified_key <- paste0(key, ".Mt")
    x[, , modified_key] <- x[, , map_kv[[key]]]
  }
  
  # Only keeping the PROM variables and dropping the rest
  x <- x[, , promnames]

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
       unit = "Mt",
       description = "Enerdata; Supplementary parameter for transfers")
}
