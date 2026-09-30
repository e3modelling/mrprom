#' readMAgPIE_BMSWAS_Price
#'
#' Read annual regional bioenergy prices prepared from a MAgPIE report for use
#' as OPEN-PROM's historical BMSWAS fuel price. The source file is already in
#' OPEN-PROM region and subsector space and uses kUSD2015/toe.
#'
#' @return The MAgPIE BMSWAS price data as a magpie object.
#'
#' @author Songmin Yu
#'
#' @examples
#' \dontrun{
#' x <- readSource("MAgPIE_BMSWAS_Price")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom tidyr pivot_longer
#' @importFrom utils read.csv
#'
readMAgPIE_BMSWAS_Price <- function() {
  sourceFile <- "iPrices_magpie.csv"
  x <- read.csv(sourceFile, check.names = FALSE)

  if (ncol(x) < 3) {
    stop(sourceFile, " must contain region, SBS, and annual price columns")
  }

  names(x)[1:2] <- c("region", "variable")
  yearColumns <- names(x)[-(1:2)]
  expectedYears <- as.character(2010:2100)

  if (!identical(yearColumns, expectedYears)) {
    stop(sourceFile, " must contain annual columns 2010 through 2100 in order")
  }
  if (nrow(x) != 39 * 34 || length(unique(x$region)) != 39 ||
      length(unique(x$variable)) != 34) {
    stop(sourceFile, " must contain exactly 39 regions x 34 SBS rows")
  }
  if (anyDuplicated(x[c("region", "variable")])) {
    stop(sourceFile, " contains duplicate region-SBS keys")
  }

  priceMatrix <- as.matrix(x[yearColumns])
  storage.mode(priceMatrix) <- "numeric"
  if (anyNA(priceMatrix) || any(!is.finite(priceMatrix)) || any(priceMatrix < 0)) {
    stop(sourceFile, " contains missing, non-finite, or negative prices")
  }

  regionVariation <- vapply(
    split(seq_len(nrow(x)), x$region),
    function(rows) max(apply(priceMatrix[rows, , drop = FALSE], 2, function(v) max(v) - min(v))),
    numeric(1)
  )
  if (any(regionVariation > 1e-12)) {
    stop(sourceFile, " contains subsector-varying prices within a region")
  }

  x <- tidyr::pivot_longer(
    x,
    cols = all_of(yearColumns),
    names_to = "period",
    values_to = "value"
  )
  x$period <- as.integer(x$period)
  x$unit <- "kUSD2015/toe"
  x <- as.quitte(x) %>% as.magpie()

  list(
    x = x,
    weight = NULL,
    description = c(
      category = "Costs",
      type = "MAgPIE bioenergy price for OPEN-PROM BMSWAS",
      filename = sourceFile,
      `Indicative size (MB)` = 1.2,
      dimensions = "3D",
      unit = "kUSD2015/toe",
      Confidential = "project"
    )
  )
}
