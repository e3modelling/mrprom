#' calcBmswasSupplyCoefGLOBIOM
#'
#' BMSWAS biomass **supply-curve** coefficients (a, b, c) for
#' P = a + b * Q^c, fitted per (GHGScen, OPEN-PROM region, decadal year) from
#' GLOBIOM's biomass supply lookup and linearly interpolated to annual
#' 2010..2100.
#'
#' Anchors and regression logic are GLOBIOM-specific and are implemented in
#' \code{toolBmswasCurveFitGLOBIOM.R}. Only generic numerical/output utilities
#' are shared with the MAgPIE emulator.
#'
#' Written by \code{fullOPEN-PROM} to \code{iBmswasSupplyCoef_globiom.csv} and
#' loaded as \code{i08BmswasSupplyCoefGlobiom(GLOBIOMSCEN, allCy,
#' GLOBIOMSUPPLYCOEF, YTIME)}.
#'
#' @return list(x = magclass [op_region, year, ghgscen.coef], weight = NULL, ...)
#' @author Songmin
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "BmswasSupplyCoefGLOBIOM", aggregate = FALSE)
#' }
#' @seealso \code{\link{calcBmswasLandEmisCoefGLOBIOM}}
#' @export
calcBmswasSupplyCoefGLOBIOM <- function() {
  anchors <- .toolBmswasLoadAnchorsGLOBIOM()
  regions <- sort(unique(anchors$op_region))
  ghgs    <- sort(unique(anchors$GHGScen))

  rows <- list()
  for (ghg in ghgs) for (r in regions) {
    dec <- .toolBmswasFitDecadalGLOBIOM(
      anchors, r, ghg, "P", .toolBmswasFitPowerLawGLOBIOM
    )
    for (k in c("a", "b", "c")) {
      ann <- .toolBmswasInterpAnnualGLOBIOM(dec[[k]])
      rows[[length(rows) + 1]] <- data.frame(
        op_region = r, ghgscen = ghg, coef = k,
        period = .toolBmswasOutYearsGLOBIOM,
        value = ann,
        stringsAsFactors = FALSE
      )
    }
  }
  df <- do.call(rbind, rows)
  x <- .toolLandUseEmulatorCoefToMagpie(
    df, keyOrder = c("ghgscen", "coef")
  )

  list(x = x, weight = NULL, isocountries = FALSE,
       unit = "a,b: US$2000/GJ-equivalent; c: dimensionless exponent",
       description = "GLOBIOM biomass supply curve P = a + b*Q^c coefficients (per GHGScen, region)")
}
