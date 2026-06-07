#' calcBmswasEmisCoefGLOBIOM
#'
#' BMSWAS land-use **emission-curve** coefficients (ea, eb, ec) for
#' Em = ea + eb * Q + ec * Q^2, per (GHGScen, OPEN-PROM region, emission type),
#' interpolated to annual 2010..2100. Split:
#'
#' \itemize{
#'   \item \strong{CO2LandUse} -- REGRESSED on Q (stock+change total):
#'     non-EU uses lookup \code{Emissions|CO2|Land Use} (change) plus the
#'     scenario-fixed Secondary-Forest stock sink (File2, added to the
#'     intercept); EU uses lookup \code{TOTAL_EMIS} (full LULUCF, CO2eq).
#'   \item \strong{CH4LandUse / N2OLandUse} -- NOT regressed (Q-independent,
#'     eb = ec = 0): non-EU uses the BioScen mean of lookup
#'     \code{Emissions|{CH4,N2O}|Land Use}; EU uses MAgPIE agriculture from a
#'     single convergence run (File1).
#' }
#' EU \code{TOTAL_EMIS} (LULUCF) and EU agriculture (File1) are disjoint, so no
#' double-count. File1 N2O is already kt, CH4/CO2 Mt (chain-consistent units).
#'
#' Anchors come from \code{readSource("GLOBIOM_LookupTable")} via
#' \code{.toolBmswasLoadAnchorsGLOBIOM}; the curve fit is source-agnostic and shared.
#'
#' Written by \code{fullOPEN-PROM} to \code{iBmswasEmisCoef_globiom.csv} and
#' loaded as \code{imBmswasEmisCoef(GHGSCEN, allCy, EMTYPE, ECOEF, YTIME)}.
#'
#' @param legacyUnitBug if TRUE keep the EJ-fitted (buggy) Q units; default FALSE
#'   fits on Q in Mtoe (see \code{\link{.toolBmswasFitQuadratic}}).
#' @return list(x = magclass [op_region, year, ghgscen.emtype.ecoef], ...)
#' @author Songmin
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "BmswasEmisCoefGLOBIOM", aggregate = FALSE)
#' }
#' @seealso \code{\link{calcBmswasSupplyCoefGLOBIOM}}, \code{\link{.toolBmswasFitQuadratic}}
#' @importFrom madrat getConfig toolGetMapping
#' @importFrom stats setNames
#' @importFrom utils read.csv
#' @export
calcBmswasEmisCoefGLOBIOM <- function(legacyUnitBug = FALSE) {
  anchors <- .toolBmswasLoadAnchorsGLOBIOM(legacyUnitBug = legacyUnitBug)
  regions <- sort(unique(anchors$op_region))
  ghgs    <- sort(unique(anchors$GHGScen))

  # EU member regions (carry TOTAL_EMIS, no gas split/agriculture in the lookup;
  # the other OP regions are non-EU). Use the package EU28 mapping.
  eu28 <- toolGetMapping("EU28.csv", type = "regional", where = "mrprom")[["ISO3.Code"]]

  # File1: EU agriculture CH4/N2O (annual, scenario-fixed). File2: non-EU
  # Secondary-Forest stock-sink CO2 (annual, scenario-fixed). Both ship in the
  # GLOBIOM_LookupTable source folder (next to GLOBIOM_LookupTable.xlsx); only the
  # region mapping is kept in the package (read via toolGetMapping in the loader).
  globiomSrc <- file.path(getConfig("sourcefolder"), "GLOBIOM_LookupTable")
  readAnnual <- function(name) {
    d <- read.csv(file.path(globiomSrc, name), stringsAsFactors = FALSE,
                         check.names = FALSE)
    yc <- grep("^X?[0-9]{4}$", names(d), value = TRUE)
    yrs <- as.integer(sub("^X", "", yc))
    list(d = d, yc = yc, yrs = yrs)
  }
  f1 <- readAnnual("eu_agriculture_ch4_n2o.csv")        # cols: region, emtype, ...
  f2 <- readAnnual("noneu_stock_forest_sink_co2.csv")   # cols: region, emtype(CO2LandUse), ...
  annualFromFile <- function(f, reg, emtype = NULL) {
    sel <- f$d$region == reg & (is.null(emtype) | f$d$emtype == emtype)
    if (!any(sel)) return(NULL)
    v <- as.numeric(f$d[which(sel)[1], f$yc])
    setNames(v, f$yrs)[as.character(.toolBmswasOutYears)]
  }

  rows <- list()
  emit <- function(ghg, r, et, coefMat) {                            # coefMat: named list ea/eb/ec (each length 91)
    for (k in c("ea", "eb", "ec"))
      rows[[length(rows) + 1]] <<- data.frame(
        op_region = r, ghgscen = ghg, emtype = et, ecoef = k,
        period = .toolBmswasOutYears, value = coefMat[[k]], stringsAsFactors = FALSE)
  }
  zero <- rep(0, length(.toolBmswasOutYears))

  for (ghg in ghgs) for (r in regions) {
    isEU <- r %in% eu28

    ## --- CO2LandUse: REGRESS Q -> (ea, eb, ec) ---
    co2col <- if (isEU) "TOTAL_EMIS" else "CO2LandUse"
    dec <- .toolBmswasFitDecadal(anchors, r, ghg, co2col, .toolBmswasFitQuadratic)
    ea <- .toolBmswasInterpAnnual(dec$a)
    eb <- .toolBmswasInterpAnnual(dec$b)
    ec <- .toolBmswasInterpAnnual(dec$c)
    if (!isEU) {                              # add the scenario-fixed stock sink to the intercept
      sink <- annualFromFile(f2, r)
      if (!is.null(sink)) ea <- ea + ifelse(is.na(sink), 0, sink)
    }
    emit(ghg, r, "CO2LandUse", list(ea = ea, eb = eb, ec = ec))

    ## --- CH4 / N2O: CONSTANT (eb = ec = 0) ---
    for (et in c("CH4LandUse", "N2OLandUse")) {
      if (isEU) {
        ea <- annualFromFile(f1, r, et)                 # MAgPIE agriculture, single run
        if (is.null(ea)) ea <- zero
        ea[is.na(ea)] <- 0
      } else {
        ea <- .toolBmswasInterpAnnual(.toolBmswasDecadalMean(anchors, r, ghg, et))
      }
      emit(ghg, r, et, list(ea = ea, eb = zero, ec = zero))
    }
  }
  df <- do.call(rbind, rows)
  x <- .toolBmswasToMagpie(df, keyOrder = c("ghgscen", "emtype", "ecoef"))

  list(x = x, weight = NULL, isocountries = FALSE,
       unit = "ea,eb,ec for Em=ea+eb*Q+ec*Q^2 (CO2 Mt; CH4 Mt; N2O kt per yr)",
       description = "GLOBIOM land-use emission curve coefficients: CO2 regressed (stock+change), CH4/N2O constant (agriculture)")
}
