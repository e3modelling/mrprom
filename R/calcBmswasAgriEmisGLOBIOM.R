#' calcBmswasAgriEmisGLOBIOM
#'
#' BMSWAS AFOLU **agriculture** CH4/N2O emissions. These are Q-INDEPENDENT (their
#' across-BioScen variation is <1\%), so they are shipped as a direct
#' (GHGScen, region, gas, year) table rather than a fitted curve. EU uses MAgPIE
#' agriculture (File1, a single convergence run); non-EU uses the BioScen mean of
#' GLOBIOM lookup \code{Emissions|{CH4,N2O}|Land Use}. Interpolated to annual
#' 2010..2100. (Land CO2, which IS Q-dependent, is in
#' \code{\link{calcBmswasLandEmisCoefGLOBIOM}}.)
#'
#' Written by \code{fullOPEN-PROM} to \code{iBmswasAgriEmis_globiom.csv} and loaded
#' directly as \code{imBmswasAgriEmis(GHGSCEN, allCy, EMTYPE, YTIME)} (no curve).
#'
#' @return list(x = magclass [op_region, year, ghgscen.emtype], ...)
#' @author Songmin
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "BmswasAgriEmisGLOBIOM", aggregate = FALSE)
#' }
#' @seealso \code{\link{calcBmswasLandEmisCoefGLOBIOM}}, \code{\link{.toolBmswasDecadalMean}}
#' @importFrom madrat readSource toolGetMapping
#' @importFrom magclass getItems
#' @export
calcBmswasAgriEmisGLOBIOM <- function() {
  anchors <- .toolBmswasLoadAnchorsGLOBIOM()
  regions <- sort(unique(anchors$op_region))
  ghgs    <- sort(unique(anchors$GHGScen))
  eu28 <- toolGetMapping("EU28.csv", type = "regional", where = "mrprom")[["ISO3.Code"]]

  # File1: EU agriculture CH4/N2O (MAgPIE single run), read via readSource subtype
  f1 <- readSource("GLOBIOM_LookupTable", subtype = "euAgriculture", convert = FALSE)
  f1yr <- paste0("y", .toolBmswasOutYears)
  euAgri <- function(reg, et) {
    if (!(reg %in% getItems(f1, 1)) || !(et %in% getItems(f1, 3))) return(NULL)
    as.numeric(f1[reg, f1yr, et])
  }
  zero <- rep(0, length(.toolBmswasOutYears))

  rows <- list()
  for (ghg in ghgs) for (r in regions) {
    isEU <- r %in% eu28
    for (et in c("CH4LandUse", "N2OLandUse")) {
      val <- if (isEU) euAgri(r, et) else .toolBmswasInterpAnnual(.toolBmswasDecadalMean(anchors, r, ghg, et))
      if (is.null(val)) val <- zero
      val[is.na(val)] <- 0
      rows[[length(rows) + 1]] <- data.frame(
        op_region = r, ghgscen = ghg, emtype = et,
        period = .toolBmswasOutYears, value = val, stringsAsFactors = FALSE)
    }
  }
  df <- do.call(rbind, rows)
  x <- .toolBmswasToMagpie(df, keyOrder = c("ghgscen", "emtype"))

  list(x = x, weight = NULL, isocountries = FALSE,
       unit = "CH4 Mt/yr; N2O kt/yr",
       description = "AFOLU agriculture CH4/N2O emissions, Q-independent (EU=MAgPIE File1; non-EU=GLOBIOM BioScen mean)")
}
