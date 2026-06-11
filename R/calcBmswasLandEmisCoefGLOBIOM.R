#' calcBmswasLandEmisCoefGLOBIOM
#'
#' BMSWAS land-use **CO2** emission-curve coefficients (ea, eb, ec) for
#' Em = ea + eb * Q + ec * Q^2, per (GHGScen, OPEN-PROM region), interpolated to
#' annual 2010..2100. CO2 land ONLY -- it is Q-dependent (regressed). Agriculture
#' CH4/N2O are Q-independent and shipped separately as a direct table by
#' \code{\link{calcBmswasAgriEmisGLOBIOM}}.
#'
#' Both EU and non-EU regress a FULL-SCOPE land CO2 total on Q (symmetric), but the
#' lookup carries different variables for each:
#' \itemize{
#'   \item \strong{EU}: \code{TOTAL_EMIS} (full LULUCF, CO2eq, BioScen-varying) --
#'     already full-scope; regressed directly. EU land CO2 is dominated by its
#'     standing-forest sink, which IS Q-correlated (~0.9), so the aggregate
#'     regression already captures that stock erosion.
#'   \item \strong{non-EU}: \code{Emissions|CO2|Land Use} (the BioScen/Q-varying
#'     land-use-change flux, incl. the deforestation pulse) PLUS the File2
#'     Secondary-Forest regrowth sink, which is folded in to complete the scope.
#'     File2 has no BioScen/GHGScen dimension, so it is replicated as-is (the same
#'     value at each year) into every anchor's CO2|Land Use before fitting -- so
#'     non-EU also carries a full-scope total, regressed the same way as EU.
#' }
#' NB on the fold: replicating a constant across BioScen only shifts the fitted
#' intercept, not the Q-slope. So this completes the SCOPE (full-scope total) but
#' does NOT capture the high-Q erosion of the regrowth sink -- File2 carries no Q
#' resolution. (Known simplification; cf. EU's standing-forest sink which, having
#' BioScen variation, does carry its Q-erosion into eb/ec.)
#'
#' Anchors come from \code{readSource("GLOBIOM_LookupTable", convert = FALSE)} via
#' \code{.toolBmswasLoadAnchorsGLOBIOM}; the curve fit is source-agnostic.
#'
#' Written by \code{fullOPEN-PROM} to \code{iBmswasLandEmisCoef_globiom.csv} and
#' loaded as \code{imBmswasLandEmisCoef(GHGSCEN, allCy, EMTYPE, ECOEF, YTIME)}.
#'
#' @param legacyUnitBug if TRUE keep the EJ-fitted (buggy) Q units; default FALSE
#'   fits on Q in Mtoe (see \code{\link{.toolBmswasFitQuadratic}}).
#' @return list(x = magclass [op_region, year, ghgscen.emtype.ecoef], ...)
#' @author Songmin
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "BmswasLandEmisCoefGLOBIOM", aggregate = FALSE)
#' }
#' @seealso \code{\link{calcBmswasAgriEmisGLOBIOM}}, \code{\link{.toolBmswasFitQuadratic}}
#' @importFrom madrat readSource toolGetMapping
#' @importFrom stats setNames
#' @export
calcBmswasLandEmisCoefGLOBIOM <- function(legacyUnitBug = FALSE) {
  anchors <- .toolBmswasLoadAnchorsGLOBIOM(legacyUnitBug = legacyUnitBug)
  eu28 <- toolGetMapping("EU28.csv", type = "regional", where = "mrprom")[["ISO3.Code"]]

  # Make non-EU land CO2 FULL-SCOPE before fitting: fold the File2 Secondary-Forest
  # regrowth sink into each non-EU anchor's CO2|Land Use (the lookup change flux).
  # File2 has no BioScen/GHGScen dimension, so it is replicated as-is across them
  # (the same value at each year). Non-EU then carries a full-scope land CO2,
  # symmetric with EU's full-scope TOTAL_EMIS, and is regressed the same way.
  # A constant added across BioScen only shifts the fitted intercept, not the
  # Q-slope -> this completes the scope, not the sink's (uncaptured) Q-dynamics.
  f2 <- readSource("GLOBIOM_LookupTable", subtype = "noneuStockSink", convert = FALSE)
  f2df <- as.data.frame(f2, rev = 2)
  names(f2df)[ncol(f2df)] <- "sink"
  sinkLU <- setNames(f2df$sink, paste(as.character(f2df[[1]]),
                     as.integer(sub("^y", "", as.character(f2df$year))), sep = "|"))
  ai <- which(!(anchors$op_region %in% eu28) & is.finite(anchors$CO2LandUse))
  add <- sinkLU[paste(anchors$op_region[ai], anchors$Year[ai], sep = "|")]
  anchors$CO2LandUse[ai] <- anchors$CO2LandUse[ai] + ifelse(is.na(add), 0, add)

  regions <- sort(unique(anchors$op_region))
  ghgs    <- sort(unique(anchors$GHGScen))
  rows <- list()
  for (ghg in ghgs) for (r in regions) {
    co2col <- if (r %in% eu28) "TOTAL_EMIS" else "CO2LandUse"   # both full-scope now
    dec <- .toolBmswasFitDecadal(anchors, r, ghg, co2col, .toolBmswasFitQuadratic)
    co <- list(ea = .toolBmswasInterpAnnual(dec$a),
               eb = .toolBmswasInterpAnnual(dec$b),
               ec = .toolBmswasInterpAnnual(dec$c))
    for (k in c("ea", "eb", "ec"))
      rows[[length(rows) + 1]] <- data.frame(
        op_region = r, ghgscen = ghg, emtype = "CO2LandUse", ecoef = k,
        period = .toolBmswasOutYears, value = co[[k]], stringsAsFactors = FALSE)
  }
  df <- do.call(rbind, rows)
  x <- .toolBmswasToMagpie(df, keyOrder = c("ghgscen", "emtype", "ecoef"))

  list(x = x, weight = NULL, isocountries = FALSE,
       unit = "ea,eb,ec for Em=ea+eb*Q+ec*Q^2 (Mt CO2/yr)",
       description = "GLOBIOM full-scope land CO2 emission curve coefficients (EU=TOTAL_EMIS; non-EU=CO2|Land Use change flux + Secondary-Forest regrowth sink), regressed on Q")
}
