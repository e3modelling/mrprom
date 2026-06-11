#' calcBmswasSupplyCoefGLOBIOM
#'
#' BMSWAS biomass **supply-curve** coefficients (a, b, c) for
#' P = a + b * Q^c, fitted per (GHGScen, OPEN-PROM region, decadal year) from
#' GLOBIOM's biomass supply lookup and linearly interpolated to annual
#' 2010..2100. Replaces the supply half of the retired
#' \code{scripts/tools/build_bmswas_coef_globiom.py}.
#'
#' Anchors come from \code{readSource("GLOBIOM_LookupTable", convert = FALSE)} via
#' \code{.toolBmswasLoadAnchorsGLOBIOM}; the fit (\code{.toolBmswasFitPowerlaw} etc.)
#' is source-agnostic and shared.
#'
#' Written by \code{fullOPEN-PROM} to \code{iBmswasSupplyCoef_globiom.csv} and
#' loaded as \code{imBmswasSupplyCoef(GHGSCEN, allCy, COEF, YTIME)}.
#'
#' @param legacyUnitBug if TRUE keep the EJ-fitted (buggy) Q units; default FALSE
#'   fits on Q in Mtoe (see \code{\link{.toolBmswasFitPowerlaw}}).
#' @return list(x = magclass [op_region, year, ghgscen.coef], weight = NULL, ...)
#' @author Songmin
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "BmswasSupplyCoefGLOBIOM", aggregate = FALSE)
#' }
#' @seealso \code{\link{calcBmswasLandEmisCoefGLOBIOM}}, \code{\link{.toolBmswasFitPowerlaw}}
#' @export
calcBmswasSupplyCoefGLOBIOM <- function(legacyUnitBug = FALSE) {
  anchors <- .toolBmswasLoadAnchorsGLOBIOM(legacyUnitBug = legacyUnitBug)
  regions <- sort(unique(anchors$op_region))
  ghgs    <- sort(unique(anchors$GHGScen))

  rows <- list()
  for (ghg in ghgs) for (r in regions) {
    dec <- .toolBmswasFitDecadal(anchors, r, ghg, "P", .toolBmswasFitPowerlaw)
    for (k in c("a", "b", "c")) {
      ann <- .toolBmswasInterpAnnual(dec[[k]])
      rows[[length(rows) + 1]] <- data.frame(
        op_region = r, ghgscen = ghg, coef = k,
        period = .toolBmswasOutYears, value = ann, stringsAsFactors = FALSE)
    }
  }
  df <- do.call(rbind, rows)
  x <- .toolBmswasToMagpie(df, keyOrder = c("ghgscen", "coef"))

  list(x = x, weight = NULL, isocountries = FALSE,
       unit = "a,b: US$2000/GJ-equivalent; c: dimensionless exponent",
       description = "GLOBIOM biomass supply curve P = a + b*Q^c coefficients (per GHGScen, region)")
}
