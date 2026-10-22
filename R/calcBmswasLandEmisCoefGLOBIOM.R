#' calcBmswasLandEmisCoefGLOBIOM
#'
#' BMSWAS land-use CO2 emission-curve coefficients (ea, eb) for Em = ea + eb * Q,
#' per (GHGScen, OPEN-PROM region), interpolated to annual 2010..2100. Land CO2 is
#' Q-dependent and regressed linearly on the biomass feedstock quantity Q (Mtoe).
#' EU regresses TOTAL_EMIS (full LULUCF); non-EU regresses Emissions|CO2|Land Use.
#' Agriculture CH4/N2O are Q-independent and shipped separately by
#' \code{\link{calcBmswasAgriEmisGLOBIOM}}.
#'
#' Anchors, regional treatment, and regression logic are GLOBIOM-specific and
#' are implemented in \code{toolBmswasCurveFitGLOBIOM.R}.
#'
#' Written by \code{fullOPEN-PROM} to \code{iBmswasLandEmisCoef_globiom.csv} and
#' loaded as \code{i08LandCO2CoefGlobiom(GLOBIOMSCEN, allCy, EMTYPE,
#' GLOBIOMEMISCOEF, YTIME)}.
#'
#' @return list(x = magclass [op_region, year, ghgscen.emtype.ecoef], ...)
#' @author Songmin
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "BmswasLandEmisCoefGLOBIOM", aggregate = FALSE)
#' }
#' @seealso \code{\link{calcBmswasAgriEmisGLOBIOM}}
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
    dec <- .toolBmswasFitDecadalGLOBIOM(
      anchors, r, ghg, co2col, .toolBmswasFitLinearGLOBIOM
    )
    co <- list(
      ea = .toolBmswasInterpAnnualGLOBIOM(dec$a),
      eb = .toolBmswasInterpAnnualGLOBIOM(dec$b)
    )
    for (k in c("ea", "eb"))
      rows[[length(rows) + 1]] <- data.frame(
        op_region = r, ghgscen = ghg, emtype = "CO2LandUse", ecoef = k,
        period = .toolBmswasOutYearsGLOBIOM,
        value = co[[k]], stringsAsFactors = FALSE
      )
  }
  df <- do.call(rbind, rows)
  x <- .toolLandUseEmulatorCoefToMagpie(
    df, keyOrder = c("ghgscen", "emtype", "ecoef")
  )

  list(x = x, weight = NULL, isocountries = FALSE,
       unit = "ea,eb for Em=ea+eb*Q (Mt CO2/yr)",
       description = paste(
         "GLOBIOM land CO2 coefficients regressed on biomass quantity Q;",
         "EU=TOTAL_EMIS, non-EU=Emissions|CO2|Land Use"
       ))
}
