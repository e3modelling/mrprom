#' calcBmswasBioPriceH12MAgPIE
#'
#' Fit the MAgPIE H12 bioenergy-price response. For every
#' policy scenario, H12 region, and source year, native
#' \code{Prices|Bioenergy} is regressed on the requested second-generation
#' bioenergy quantity:
#'
#' \deqn{P = pa + pb Q + pc Q^2.}
#'
#' \code{Q} is in Mtoe/yr. \code{P} is converted before fitting from
#' USD2017/GJ to kUSD2015/toe using the same conversion as the backward
#' soft-link. The output also carries \code{qmin} and \code{qmax}, the fitted
#' cell's training domain. All fields are linearly interpolated to annual
#' 2010--2100 values. \code{fullOPEN-PROM} writes the result to
#' \code{iBmswasBioPriceH12_magpie.csv}, loaded by GAMS as
#' \code{i08BmswasPriceH12Magpie}.
#'
#' @return list(x = magclass [H12 region, year, ghgscen.pfield], ...)
#' @author Songmin
#' @examples
#' \dontrun{
#' x <- calcOutput("BmswasBioPriceH12MAgPIE", aggregate = FALSE)
#' }
#' @seealso \code{\link{calcBmswasLandEmisCoefMAgPIE}},
#'   \code{\link{calcBmswasAgriEmisCoefMAgPIE}}
#' @export
calcBmswasBioPriceH12MAgPIE <- function() {
  # Fit one response per H12 market; EUR uses total requested EUR Q.
  anchors <- toolBmswasLoadPriceAnchorsH12MAgPIE()

  fitPositiveQuadratic <- function(Q, Y) {
    co <- toolMagpieFitQuadratic(Q, Y)
    qr <- range(Q)
    candidates <- qr
    if (co[3] > 0) {
      vertex <- -co[2] / (2 * co[3])
      if (is.finite(vertex) && vertex > qr[1] && vertex < qr[2]) {
        candidates <- c(candidates, vertex)
      }
    }
    pred <- co[1] + co[2] * candidates + co[3] * candidates^2
    if (all(is.finite(pred)) && min(pred) > 0) return(co)

    lin <- .toolLandUseEmulatorLstsq(cbind(1, Q), Y)
    predLin <- lin[1] + lin[2] * qr
    if (all(is.finite(predLin)) && min(predLin) > 0) {
      return(c(lin[1], lin[2], 0))
    }
    c(mean(Y), 0, 0)
  }

  fits <- toolMagpieFitCells(
    anchors,
    target = "P",
    fitf = fitPositiveQuadratic
  )
  coefRows <- toolMagpieAnnualCoefficientRows(
    fits, c("pa", "pb", "pc")
  )
  names(coefRows)[names(coefRows) == "coef"] <- "pfield"

  cells <- unique(anchors[c("op_region", "GHGScen", "Year")])
  ranges <- vector("list", nrow(cells))
  for (i in seq_len(nrow(cells))) {
    use <- anchors$op_region == cells$op_region[i] &
           anchors$GHGScen == cells$GHGScen[i] &
           anchors$Year == cells$Year[i]
    qr <- range(anchors$Q[use])
    ranges[[i]] <- data.frame(
      op_region = cells$op_region[i],
      ghgscen = cells$GHGScen[i],
      period = cells$Year[i],
      qmin = qr[1],
      qmax = qr[2],
      stringsAsFactors = FALSE
    )
  }
  ranges <- do.call(rbind, ranges)

  keys <- unique(ranges[c("op_region", "ghgscen")])
  rangeRows <- list()
  for (i in seq_len(nrow(keys))) {
    z <- ranges[
      ranges$op_region == keys$op_region[i] &
      ranges$ghgscen == keys$ghgscen[i], ]
    for (field in c("qmin", "qmax")) {
      rangeRows[[length(rangeRows) + 1L]] <- data.frame(
        op_region = keys$op_region[i],
        ghgscen = keys$ghgscen[i],
        pfield = field,
        period = .toolMagpieOutYears,
        value = toolMagpieInterpAnnual(z$period, z[[field]]),
        stringsAsFactors = FALSE
      )
    }
  }

  df <- rbind(
    coefRows[c("op_region", "ghgscen", "pfield", "period", "value")],
    do.call(rbind, rangeRows)
  )
  x <- .toolLandUseEmulatorCoefToMagpie(
    df, keyOrder = c("ghgscen", "pfield")
  )

  list(
    x = x,
    weight = NULL,
    isocountries = FALSE,
    unit = "P: kUSD2015/toe; Q/qmin/qmax: Mtoe/yr",
    description = paste(
      "MAgPIE H12 native bioenergy absolute price",
      "P=pa+pb*Q+pc*Q^2 fitted against requested coupling demand"
    )
  )
}
