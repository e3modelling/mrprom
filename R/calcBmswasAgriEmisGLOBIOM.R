#' calcBmswasAgriEmisGLOBIOM
#'
#' BMSWAS AFOLU **agriculture** CH4/N2O emissions. These are Q-INDEPENDENT (their
#' across-BioScen variation is <1\%), so they are shipped as a direct
#' (GHGScen, region, gas, year) table rather than a fitted curve. Because the
#' GLOBIOM workbook has no EU28 agriculture CH4/N2O rows, the GLOBIOM emulator
#' uses its dedicated MAgPIE-derived annual supplement for EU28; non-EU uses the
#' BioScen mean of GLOBIOM lookup \code{Emissions|{CH4,N2O}|Land Use}. The
#' supplement is not part of the MAgPIE emulator regression pipeline.
#' Values are interpolated to annual
#' 2010..2100. (Land CO2, which IS Q-dependent, is in
#' \code{\link{calcBmswasLandEmisCoefGLOBIOM}}.)
#'
#' Written by \code{fullOPEN-PROM} to \code{iBmswasAgriEmis_globiom.csv} and loaded
#' directly as \code{i08AgriEmisGlobiom(GLOBIOMSCEN, allCy, EMTYPE, YTIME)}
#' (no curve).
#'
#' @return list(x = magclass [op_region, year, ghgscen.emtype], ...)
#' @author Songmin
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "BmswasAgriEmisGLOBIOM", aggregate = FALSE)
#' }
#' @seealso \code{\link{calcBmswasLandEmisCoefGLOBIOM}}
#' @importFrom madrat readSource toolGetMapping
#' @export
calcBmswasAgriEmisGLOBIOM <- function() {
  anchors <- .toolBmswasLoadAnchorsGLOBIOM()
  regions <- sort(unique(anchors$op_region))
  ghgs    <- sort(unique(anchors$GHGScen))
  eu28 <- toolGetMapping("EU28.csv", type = "regional", where = "mrprom")[["ISO3.Code"]]

  euAgriSupplement <- .toolBmswasLoadAgriSupplementGLOBIOM()
  zero <- rep(0, length(.toolBmswasOutYearsGLOBIOM))

  rows <- list()
  for (ghg in ghgs) for (r in regions) {
    isEU <- r %in% eu28
    for (et in c("CH4LandUse", "N2OLandUse")) {
      val <- if (isEU) {
        .toolBmswasAgriSupplementSeriesGLOBIOM(
          euAgriSupplement, r, et
        )
      } else {
        .toolBmswasInterpAnnualGLOBIOM(
          .toolBmswasDecadalMeanGLOBIOM(anchors, r, ghg, et)
        )
      }
      if (is.null(val)) val <- zero
      val[is.na(val)] <- 0
      rows[[length(rows) + 1]] <- data.frame(
        op_region = r, ghgscen = ghg, emtype = et,
        period = .toolBmswasOutYearsGLOBIOM,
        value = val,
        stringsAsFactors = FALSE
      )
    }
  }
  df <- do.call(rbind, rows)
  x <- .toolLandUseEmulatorCoefToMagpie(
    df, keyOrder = c("ghgscen", "emtype")
  )

  list(x = x, weight = NULL, isocountries = FALSE,
       unit = "CH4 Mt/yr; N2O kt/yr",
       description = paste(
         "GLOBIOM-emulator AFOLU agriculture CH4/N2O, Q-independent;",
         "EU=dedicated MAgPIE-derived supplement, non-EU=GLOBIOM BioScen mean"
       ))
}
