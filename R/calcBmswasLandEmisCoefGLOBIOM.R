#' calcBmswasLandEmisCoefGLOBIOM
#'
#' BMSWAS land-use CO2 emission-curve coefficients (ea, eb) for Em = ea + eb * Q,
#' per (GHGScen, OPEN-PROM region), interpolated to annual 2010..2100. Land CO2 is
#' Q-dependent and regressed linearly on the biomass feedstock quantity Q (Mtoe).
#' EU regresses TOTAL_EMIS (full LULUCF); non-EU regresses Emissions|CO2|Land Use.
#' Agriculture CH4/N2O are Q-independent and shipped separately by
#' \code{\link{calcBmswasAgriEmisGLOBIOM}}.
#'
#' Anchors come from \code{readSource("GLOBIOM_LookupTable", convert = FALSE)} via
#' \code{.toolBmswasLoadAnchorsGLOBIOM}; the curve fit is source-agnostic.
#'
#' Written by \code{fullOPEN-PROM} to \code{iBmswasLandEmisCoef_globiom.csv} and
#' loaded as \code{imBmswasLandEmisCoef(GHGSCEN, allCy, EMTYPE, ECOEF, YTIME)}.
#'
#' @return list(x = magclass [op_region, year, ghgscen.emtype.ecoef], ...)
#' @author Songmin
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "BmswasLandEmisCoefGLOBIOM", aggregate = FALSE)
#' }
#' @seealso \code{\link{calcBmswasAgriEmisGLOBIOM}}, \code{\link{.toolBmswasFitLinear}}
#' @importFrom madrat readSource toolGetMapping
#' @export
calcBmswasLandEmisCoefGLOBIOM <- function() {
  anchors <- .toolBmswasLoadAnchorsGLOBIOM()
  eu28 <- toolGetMapping("EU28.csv", type = "regional", where = "mrprom")[["ISO3.Code"]]

  regions <- sort(unique(anchors$op_region))
  ghgs    <- sort(unique(anchors$GHGScen))
  rows <- list()
  for (ghg in ghgs) for (r in regions) {
    co2col <- if (r %in% eu28) "TOTAL_EMIS" else "CO2LandUse"
    dec <- .toolBmswasFitDecadal(anchors, r, ghg, co2col, .toolBmswasFitLinear)
    co <- list(ea = .toolBmswasInterpAnnual(dec$a),
               eb = .toolBmswasInterpAnnual(dec$b))
    for (k in c("ea", "eb"))
      rows[[length(rows) + 1]] <- data.frame(
        op_region = r, ghgscen = ghg, emtype = "CO2LandUse", ecoef = k,
        period = .toolBmswasOutYears, value = co[[k]], stringsAsFactors = FALSE)
  }
  df <- do.call(rbind, rows)
  x <- .toolBmswasToMagpie(df, keyOrder = c("ghgscen", "emtype", "ecoef"))

  list(x = x, weight = NULL, isocountries = FALSE,
       unit = "ea,eb for Em=ea+eb*Q (Mt CO2/yr)",
       description = "GLOBIOM land CO2 emission curve coefficients (EU=TOTAL_EMIS; non-EU=Emissions|CO2|Land Use), linearly regressed on biomass quantity Q")
}
