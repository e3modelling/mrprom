#' toolBmswasCurveFitGLOBIOM
#'
#' GLOBIOM-specific fitting helpers for the BMSWAS emulator coefficient tables
#' (supply curve P = a + b*Q^c and emission curve Em = ea + eb*Q),
#' built exclusively from GLOBIOM's biomass supply lookup. Used by
#' \code{\link{calcBmswasSupplyCoefGLOBIOM}} and \code{\link{calcBmswasLandEmisCoefGLOBIOM}}.
#'
#' Supply coefficients use constrained nonlinear least squares; land-emission
#' coefficients use signed ordinary least squares. Rank-deficient cells are
#' handled by the common minimum-norm least-squares utility.
#'
#' These helpers are intentionally separate from the MAgPIE emulator because
#' the two sources use different quantities, regional resolutions, regression
#' forms, and scenario labels.
#'
#' @author Songmin
#' @importFrom madrat readSource toolGetMapping
#' @importFrom minpack.lm nlsLM nls.lm.control
#' @importFrom stats aggregate approx coef reshape
#' @name toolBmswasCurveFitGLOBIOM
#' @keywords internal
NULL

.toolBmswasFeedstockVarsGLOBIOM <- c(
  "Primary Energy|Biomass|Energy Crops",
  "Primary Energy|Biomass|Forest industry residues",
  "Primary Energy|Biomass|Fuelwood",
  "Primary Energy|Biomass|Logging residues",
  "Primary Energy|Biomass|Other Solid",
  "Primary Energy|Biomass|Roundwood harvest")
.toolBmswasPriceVarGLOBIOM <- "Price|Primary Energy|Biomass"
.toolBmswasEmVarsGLOBIOM <- c(CO2LandUse = "Emissions|CO2|Land Use",
                      CH4LandUse = "Emissions|CH4|Land Use",
                      N2OLandUse = "Emissions|N2O|Land Use")
.toolBmswasOutYearsGLOBIOM        <- 2010:2100
.toolBmswasSrcYearsDecadalGLOBIOM <- seq(2010, 2070, 10)
.toolBmswasQFloorGLOBIOM <- 1e-9

#' @description Build one anchor row per (op_region, GHGScen, Year, BioScen)
#'   after cross-region aggregation. \code{long} is the melted lookup with
#'   columns Region, Variable, BioScen, GHGScen, Year, Value; \code{mapping} is
#'   a named character vector globiom_region -> op_region.
.toolBmswasAnchorsGLOBIOM <- function(long, mapping) {
  q <- long[long$Variable %in% .toolBmswasFeedstockVarsGLOBIOM, ]
  q <- aggregate(Value ~ Region + GHGScen + Year + BioScen, q,
                        FUN = function(v) sum(v, na.rm = TRUE))
  names(q)[names(q) == "Value"] <- "Q"
  p <- long[long$Variable == .toolBmswasPriceVarGLOBIOM,
            c("Region", "GHGScen", "Year", "BioScen", "Value")]
  names(p)[names(p) == "Value"] <- "P"
  # carry the 3 gas Land-Use vars (non-EU) plus TOTAL_EMIS (EU LULUCF, CO2eq)
  allEm <- c(.toolBmswasEmVarsGLOBIOM, TOTAL_EMIS = "TOTAL_EMIS")
  em <- long[long$Variable %in% allEm, ]
  em$EmType <- names(allEm)[match(em$Variable, allEm)]
  em <- aggregate(Value ~ Region + GHGScen + Year + BioScen + EmType, em,
                        FUN = function(v) sum(v, na.rm = TRUE))
  emw <- reshape(em, idvar = c("Region", "GHGScen", "Year", "BioScen"),
                        timevar = "EmType", direction = "wide")
  names(emw) <- sub("^Value\\.", "", names(emw))

  df <- merge(q, p, by = c("Region", "GHGScen", "Year", "BioScen"))
  df <- merge(df, emw, by = c("Region", "GHGScen", "Year", "BioScen"), all.x = TRUE)
  df$op_region <- mapping[as.character(df$Region)]
  df <- df[!is.na(df$op_region), ]
  df$PxQ <- df$P * df$Q
  emcols <- names(allEm)
  for (c0 in emcols) if (!c0 %in% names(df)) df[[c0]] <- NA_real_

  agg <- aggregate(
    cbind(Q, PxQ) ~ op_region + GHGScen + Year + BioScen, df, FUN = sum)
  emagg <- aggregate(
    df[emcols], by = df[c("op_region", "GHGScen", "Year", "BioScen")],
    FUN = function(v) sum(v))                          # NA-propagating (matches py left-join sum)
  agg <- merge(agg, emagg, by = c("op_region", "GHGScen", "Year", "BioScen"))
  agg$P <- ifelse(agg$Q > .toolBmswasQFloorGLOBIOM, agg$PxQ / agg$Q, NA_real_)
  agg$PxQ <- NULL
  agg[agg$Q > .toolBmswasQFloorGLOBIOM & !is.na(agg$P) & agg$P >= 0, ]
}

#' @description Supply fit Y = a + b*Q^c, a,b>=0, 0.1<=c<=5. Multistart
#'   Levenberg-Marquardt (minpack.lm) with a flat (b=0) robustness fallback:
#'   a real fit must beat the flat one, else (divergence / unidentifiable
#'   degenerate anchors) the flat fit is returned.
.toolBmswasFitPowerLawGLOBIOM <- function(Q, Y) {
  n <- length(Q)
  ssrOf <- function(p) sum((p[1] + p[2] * Q^p[3] - Y)^2)
  best <- c(mean(Y), 0, 1); bestSsr <- ssrOf(best)
  if (n >= 3) {
    d <- data.frame(Q = Q, Y = Y)
    linb <- tryCatch(max(0, .toolLandUseEmulatorLstsq(cbind(1, Q), Y)[2]),
                     error = function(e) 1)
    starts <- list(c(0, 1, 1), c(mean(Y), 0, 1), c(0, linb, 1),
                   c(0, 1, 0.5), c(0, 1, 2), c(0, 1, 3))
    for (s0 in starts) {
      res <- tryCatch(
        nlsLM(Y ~ a + b * Q^c, data = d,
          start = c(a = s0[1], b = s0[2], c = s0[3]),
          lower = c(a = 0, b = 0, c = 0.1), upper = c(a = Inf, b = Inf, c = 5),
          control = nls.lm.control(maxiter = 1000, maxfev = 5000)),
        error = function(e) NULL)
      if (!is.null(res)) {
        abc <- as.numeric(coef(res)); ssr <- ssrOf(abc)
        if (is.finite(ssr) && ssr < bestSsr - 1e-12) { best <- abc; bestSsr <- ssr }
      }
    }
    return(best)
  }
  if (n >= 2) {
    co <- .toolLandUseEmulatorLstsq(cbind(1, Q), Y)
    return(c(max(0, co[1]), max(0, co[2]), 1))
  }
  best
}

#' @description Emission fit Y = a + b*Q (OLS). Returns (a, b, 0); the trailing 0
#'   keeps the (a, b, c) shape shared with the supply power-law fit.
.toolBmswasFitLinearGLOBIOM <- function(Q, Y) {
  if (length(Q) >= 2) {
    co <- .toolLandUseEmulatorLstsq(cbind(1, Q), Y)
    return(c(co[1], co[2], 0))
  }
  c(0, 0, 0)
}

#' @description For one (op_region, ghg) return list(a,b,c) of named (year->value)
#'   for every decadal year with >= 2 usable anchors. \code{fitf} selects the form.
.toolBmswasFitDecadalGLOBIOM <- function(anchors, op, ghg, target, fitf) {
  dec <- list(a = list(), b = list(), c = list())
  sub <- anchors[anchors$op_region == op & anchors$GHGScen == ghg, ]
  for (y in .toolBmswasSrcYearsDecadalGLOBIOM) {
    ys <- sub[sub$Year == y & !is.na(sub[[target]]), ]
    if (nrow(ys) < 2) next
    abc <- fitf(ys$Q, ys[[target]])
    dec$a[[as.character(y)]] <- abc[1]
    dec$b[[as.character(y)]] <- abc[2]
    dec$c[[as.character(y)]] <- abc[3]
  }
  dec
}

#' @description Per (op_region, ghg), the BioScen mean of \code{target} at each
#'   decadal year with any anchor — for agriculture CH4/N2O which are NOT
#'   regressed (Q-independent: ea = mean, eb = ec = 0). Returns named year->mean.
.toolBmswasDecadalMeanGLOBIOM <- function(anchors, op, ghg, target) {
  out <- list()
  sub <- anchors[anchors$op_region == op & anchors$GHGScen == ghg, ]
  for (y in .toolBmswasSrcYearsDecadalGLOBIOM) {
    v <- sub[sub$Year == y, target]
    v <- v[!is.na(v)]
    if (length(v)) out[[as.character(y)]] <- mean(v)
  }
  out
}

#' @description Linear interpolation of {year->value} onto out_years; back-fill
#'   before earliest, forward-fill after latest (so 2071..2100 inherit 2070).
.toolBmswasInterpAnnualGLOBIOM <- function(
    y2v, outYears = .toolBmswasOutYearsGLOBIOM) {
  if (length(y2v) == 0) return(rep(0, length(outYears)))
  sy <- sort(as.integer(names(y2v))); sv <- unlist(y2v[as.character(sy)])
  vapply(outYears, function(y) {
    if (y <= sy[1]) sv[1]
    else if (y >= sy[length(sy)]) sv[length(sv)]
    else approx(sy, sv, xout = y)$y
  }, numeric(1))
}

#' @description Read the lookup source, reconstruct the melted long table, load
#'   the globiom->openprom region mapping, and return the anchor table. Used by
#'   the GLOBIOM calc functions. (Assumes GLOBIOM variable/BioScen/GHGScen labels
#'   contain no "." — they don't — so the dot-joined dim3 splits into exactly 3.)
.toolBmswasLoadAnchorsGLOBIOM <- function() {
  x <- readSource("GLOBIOM_LookupTable", convert = FALSE)
  long <- as.data.frame(x, rev = 2)                 # tibble: region year variable bioscen ghgscen .value
  long <- as.data.frame(long)
  names(long)[names(long) %in% c(".value", "value", "Value")] <- "value"
  long$Region   <- as.character(long$region)
  long$Year     <- as.integer(as.character(long$year))
  long$Variable <- as.character(long$variable)
  long$BioScen  <- as.character(long$bioscen)
  long$GHGScen  <- as.character(long$ghgscen)
  long$Value    <- long$value

  map <- toolGetMapping("globiom_to_openprom.csv", type = "regional",
                                where = "mrprom")
  mapping <- stats::setNames(as.character(map$openprom_region),
                             as.character(map$globiom_region))
  anchors <- .toolBmswasAnchorsGLOBIOM(long, mapping)

  # The lookup feedstock Q is in EJ/yr; the curves are evaluated at runtime at the
  # BMSWAS quantity in Mtoe, so fit on Q in Mtoe (1 Mtoe = 0.041868 EJ).
  anchors$Q <- anchors$Q / 0.041868
  anchors
}
