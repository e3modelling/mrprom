#' Replace aggregated BMSWAS prices with MAgPIE prices
#'
#' Replaces only existing BMSWAS region-SBS-year values. The MAgPIE source is
#' expressed in kUSD2015/toe, while IFuelPrice is written in USD2015/toe.
#'
#' @param x Regionally aggregated IFuelPrice magpie object.
#'
#' @return IFuelPrice as a quitte data frame with BMSWAS values replaced.
#'
#' @importFrom madrat readSource
#' @importFrom quitte as.quitte
#'
toolReplaceBMSWASPrice <- function(x) {
  fuelPrices <- as.quitte(x)
  requiredColumns <- c("region", "variable", "new", "period", "value")
  if (!all(requiredColumns %in% names(fuelPrices))) {
    stop("Aggregated IFuelPrice is missing required dimensions")
  }

  magpiePrices <- as.quitte(readSource("MAgPIE_BMSWAS_Price"))
  if (!all(c("region", "variable", "period", "unit", "value") %in% names(magpiePrices))) {
    stop("MAgPIE BMSWAS price source is missing required dimensions")
  }
  if (!identical(unique(as.character(magpiePrices$unit)), "kUSD2015/toe")) {
    stop("MAgPIE BMSWAS prices must use kUSD2015/toe")
  }

  magpieKey <- paste(magpiePrices$region, magpiePrices$variable, magpiePrices$period, sep = "\r")
  if (anyDuplicated(magpieKey)) {
    stop("MAgPIE BMSWAS price source contains duplicate region-SBS-year keys")
  }

  replaceRows <- fuelPrices$new == "BMSWAS"
  if (!any(replaceRows)) {
    stop("Aggregated IFuelPrice contains no BMSWAS values to replace")
  }
  if (!identical(unique(as.character(fuelPrices$unit[replaceRows])), "USD2015/toe")) {
    stop("IFuelPrice BMSWAS values must use USD2015/toe before replacement")
  }
  targetKey <- paste(
    fuelPrices$region[replaceRows],
    fuelPrices$variable[replaceRows],
    fuelPrices$period[replaceRows],
    sep = "\r"
  )
  sourceRows <- match(targetKey, magpieKey)
  if (anyNA(sourceRows)) {
    missingKeys <- unique(targetKey[is.na(sourceRows)])
    stop(
      "MAgPIE BMSWAS prices do not cover all existing IFuelPrice keys; first missing key: ",
      missingKeys[[1]]
    )
  }

  fuelPrices$value[replaceRows] <- magpiePrices$value[sourceRows] * 1000
  fuelPrices
}
