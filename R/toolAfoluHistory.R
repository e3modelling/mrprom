# Common AFOLU history helpers for land-use emulators.

.toolAfoluHistoryYears <- 2010:2025
.toolAfoluHistoryAnchors <- c(2010, 2015, 2020, 2025)

toolAfoluHistoryAnnual <- function() {
  anchors <- readSource("MAgPIE_HistoricalEmissions", convert = FALSE)
  if (!setequal(
    magclass::getYears(anchors, as.integer = TRUE),
    .toolAfoluHistoryAnchors
  )) {
    stop("toolAfoluHistoryAnnual: source anchors must be 2010/2015/2020/2025")
  }

  annual <- magclass::time_interpolate(
    anchors,
    interpolated_year = .toolAfoluHistoryYears,
    extrapolation_type = "constant"
  )
  if (any(!is.finite(as.numeric(annual)))) {
    stop("toolAfoluHistoryAnnual: annual history contains non-finite values")
  }

  for (year in .toolAfoluHistoryAnchors) {
    sourceValue <- as.numeric(anchors[, paste0("y", year), ])
    annualValue <- as.numeric(annual[, paste0("y", year), ])
    if (!isTRUE(all.equal(sourceValue, annualValue, tolerance = 1e-12))) {
      stop("toolAfoluHistoryAnnual: interpolation changed source anchor ", year)
    }
  }
  magclass::getSets(annual) <- c("region", "year", "emtype")
  annual
}
