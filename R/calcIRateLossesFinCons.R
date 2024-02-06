#' calcIRateLossesFinCons
#'
#' Use enerdata and IRF data to derive OPENPROM input parameter iRateLossesFinCons
#' (distribution losses over final consumption)
#'
#' @return  OPENPROM input data iRateLossesFinCons
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IRateLossesFinCons", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIRateLossesFinCons <- function() {

  fEndHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndHorizon"]

  dl <- calcOutput("IDataDistrLosses", aggregate = FALSE)
  INDSE <- calcOutput("IFuelCons", subtype = "INDSE", aggregate = FALSE)
  TRANSE <- calcOutput("IFuelCons", subtype = "TRANSE", aggregate = FALSE)
  DOMSE <- calcOutput("IFuelCons", subtype = "DOMSE", aggregate = FALSE)
  NENSE <- calcOutput("IFuelCons", subtype = "NENSE", aggregate = FALSE)
  years <- intersect(getYears(NENSE), getYears(TRANSE))
  x <- mbind(INDSE[, years, ],
             TRANSE[, years, ],
             DOMSE[, years, ],
             NENSE[, years, ])

  fuels <- intersect(getItems(x, 3.3), getItems(dl, 3.1))
  x <- collapseNames(dl[, years, fuels]) / dimSums(collapseNames(x[, , fuels]), 3.1, na.rm = TRUE)
  x[is.infinite(x)] <- 0
  x[is.na(x)] <- 0
  # Converting to quitte object and interpolating periods
  qx <- as.quitte(x) %>% interpolate_missing_periods(expand.values = TRUE, period = min(getYears(x, as.integer = TRUE)) : fEndHorizon)
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

  list(x = x,
       weight = NULL,
       unit = "(1)",
       description = "Rate of Distribution Losses")

}
