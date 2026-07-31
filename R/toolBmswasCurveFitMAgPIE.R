#' toolBmswasCurveFitMAgPIE
#'
#' MAgPIE-specific fitting helpers used by its three emulator outputs. H12 BMSWAS prices,
#' OP39 land-use-change CO2 emissions/removals (excluding indirect land CO2 and
#' fire emissions), and OP39 agriculture CH4/N2O are fitted independently for
#' every carbon-policy scenario and lookup year.
#'
#' H12 price and land-CO2 fits use requested effective second-generation
#' biomass Q. Agriculture fits use local energy-crop plus crop-residue Q.
#' Quantities are converted from EJ/yr to Mtoe/yr.
#'
#' @author Songmin
#' @importFrom madrat readSource
#' @importFrom stats approx
#' @name toolBmswasCurveFitMAgPIE
#' @keywords internal
NULL

.toolMagpieQVars <- c(
  crops = "Primary Energy|Biomass|2nd Generation|Energy Crops",
  residues = "Primary Energy|Biomass|2nd Generation|Crop Residues",
  total = "Primary Energy|Biomass|2nd Generation"
)
.toolMagpieEmVars <- c(
  CO2LandUse = "Emissions|CO2|AFOLU|Land",
  CH4LandUse = "Emissions|CH4|AFOLU|Agriculture",
  N2OLandUse = "Emissions|N2O|AFOLU|Agriculture"
)
.toolMagpieContractVars <- c(
  QRequested = "Primary Energy|Biomass|2nd Generation|Requested",
  PNative = "Price|Primary Energy|Biomass|Native"
)
.toolMagpieOutYears <- 2010:2100
.toolMagpieEjPerMtoe <- 0.041868
.toolMagpieQFloorEj <- 1e-12
.toolMagpieUsd2017GjToKusd2015Toe <- 0.96 * 41.868 / 1000
.toolMagpieH12 <- c(
  "CAZ", "CHA", "EUR", "IND", "JPN", "LAM",
  "MEA", "NEU", "OAS", "REF", "SSA", "USA"
)
.toolMagpieEU28 <- c(
  "AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK",
  "ESP", "EST", "FIN", "FRA", "GBR", "GRC", "HRV",
  "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT",
  "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "SWE"
)
.toolMagpieOP39 <- c(setdiff(.toolMagpieH12, "EUR"), .toolMagpieEU28)
.toolMagpieScenarios <- c("Npi_Default", "NDC_LTT", "2C", "1p5C")

toolMagpieVariableFrame <- function(long, variable, valueName) {
  z <- long[long$Variable == variable,
            c("op_region", "GHGScen", "Year", "BioDem", "Value")]
  names(z)[names(z) == "Value"] <- valueName
  z
}

# Load local effective 2G Q and agriculture emissions at OP39 resolution.
toolBmswasLoadAnchorsMAgPIE <- function() {
  x <- readSource("MAgPIE_LookupTable", convert = FALSE)
  long <- as.data.frame(x, rev = 2)
  names(long)[names(long) %in% c(".value", "value", "Value")] <- "value"
  long <- as.data.frame(long)
  long$op_region <- as.character(long$region)
  long$GHGScen <- as.character(long$ghgscen)
  long$Year <- as.integer(as.character(long$year))
  long$BioDem <- as.character(long$biodem)
  long$Variable <- as.character(long$variable)
  long$Value <- suppressWarnings(as.numeric(long$value))

  vars <- c(
    .toolMagpieQVars,
    CH4LandUse = "Emissions|CH4|AFOLU|Agriculture",
    N2OLandUse = "Emissions|N2O|AFOLU|Agriculture"
  )
  pieces <- Map(
    function(variable, valueName) toolMagpieVariableFrame(long, variable, valueName),
    unname(vars),
    c("QCrop", "QResidue", "QReported", "CH4LandUse", "N2OLandUse")
  )
  ids <- c("op_region", "GHGScen", "Year", "BioDem")
  anchors <- Reduce(function(a, b) merge(a, b, by = ids, all = TRUE), pieces)

  qcols <- c("QCrop", "QResidue", "QReported")
  for (k in qcols) {
    if (any(anchors[[k]] < -.toolMagpieQFloorEj, na.rm = TRUE)) {
      stop("toolBmswasLoadAnchorsMAgPIE: negative biomass quantity in ", k)
    }
  }
  anchors$QCrop[abs(anchors$QCrop) <= .toolMagpieQFloorEj] <- 0
  anchors$QResidue[abs(anchors$QResidue) <= .toolMagpieQFloorEj] <- 0

  qsum <- anchors$QCrop + anchors$QResidue
  anchors$Q <- qsum / .toolMagpieEjPerMtoe

  # Check the reported total against the component sum used by the regression.
  tol <- 1e-3 + 1e-3 * pmax(abs(anchors$QReported), abs(qsum))
  mismatch <- is.finite(anchors$QReported) & is.finite(qsum) &
              abs(anchors$QReported - qsum) > tol
  if (any(mismatch)) {
    stop("toolBmswasLoadAnchorsMAgPIE: reported 2G total differs materially ",
         "from energy crops plus crop residues in ", sum(mismatch), " anchors")
  }

  anchors
}

# Load the variables exchanged by the OPEN-PROM <-> MAgPIE coupling:
# requested H12 bioenergy demand and MAgPIE's native Prices|Bioenergy response.
# Q is converted to Mtoe/yr and P directly to OPEN-PROM's k$2015/toe.
toolBmswasLoadPriceAnchorsH12MAgPIE <- function() {
  x <- readSource("MAgPIE_LookupTable", subtype = "h12", convert = FALSE)
  long <- as.data.frame(x, rev = 2)
  names(long)[names(long) %in% c(".value", "value", "Value")] <- "value"
  long <- as.data.frame(long)
  long$op_region <- as.character(long$region)
  long$GHGScen <- as.character(long$ghgscen)
  long$Year <- as.integer(as.character(long$year))
  long$BioDem <- as.character(long$biodem)
  long$Variable <- as.character(long$variable)
  long$Value <- suppressWarnings(as.numeric(long$value))
  long <- long[long$op_region != "World", ]

  q <- toolMagpieVariableFrame(
    long, .toolMagpieContractVars[["QRequested"]], "QRequested"
  )
  p <- toolMagpieVariableFrame(
    long, .toolMagpieContractVars[["PNative"]], "PNative"
  )
  ids <- c("op_region", "GHGScen", "Year", "BioDem")
  anchors <- merge(q, p, by = ids, all = TRUE)
  anchors$Q <- anchors$QRequested / .toolMagpieEjPerMtoe
  anchors$P <- anchors$PNative * .toolMagpieUsd2017GjToKusd2015Toe

  if (!setequal(unique(anchors$op_region), .toolMagpieH12)) {
    stop(
      "toolBmswasLoadPriceAnchorsH12MAgPIE: expected exactly H12 regions; got ",
      paste(sort(unique(anchors$op_region)), collapse = ", ")
    )
  }
  if (any(!is.finite(anchors$Q)) || any(anchors$Q < 0)) {
    stop("toolBmswasLoadPriceAnchorsH12MAgPIE: requested Q must be finite and non-negative")
  }
  if (any(!is.finite(anchors$P)) || any(anchors$P <= 0)) {
    stop("toolBmswasLoadPriceAnchorsH12MAgPIE: native price must be finite and positive")
  }

  counts <- aggregate(
    BioDem ~ op_region + GHGScen + Year,
    data = anchors,
    FUN = function(z) length(unique(z))
  )
  if (any(counts$BioDem < 3L)) {
    stop("toolBmswasLoadPriceAnchorsH12MAgPIE: fewer than three demand anchors in a fit cell")
  }
  anchors
}

# Load OP39 land-use-change CO2 emissions/removals, excluding indirect land CO2
# and fire emissions, and the requested H12 Q. Q is the demand requested by
# OPEN-PROM at H12 resolution. Each EU28 target is paired with the common EUR
# total; the eleven non-EU regions retain their one-to-one H12 quantity.
toolBmswasLoadLandCO2AnchorsH12MAgPIE <- function() {
  toLong <- function(x) {
    long <- as.data.frame(x, rev = 2)
    names(long)[names(long) %in% c(".value", "value", "Value")] <- "value"
    long <- as.data.frame(long)
    long$op_region <- as.character(long$region)
    long$GHGScen <- as.character(long$ghgscen)
    long$Year <- as.integer(as.character(long$year))
    long$BioDem <- as.character(long$biodem)
    long$Variable <- as.character(long$variable)
    long$Value <- suppressWarnings(as.numeric(long$value))
    long
  }

  opLong <- toLong(readSource(
    "MAgPIE_LookupTable", subtype = "openprom", convert = FALSE
  ))
  h12Long <- toLong(readSource(
    "MAgPIE_LookupTable", subtype = "h12", convert = FALSE
  ))
  opLong <- opLong[opLong$op_region != "World", ]
  h12Long <- h12Long[h12Long$op_region != "World", ]

  target <- toolMagpieVariableFrame(
    opLong, .toolMagpieEmVars[["CO2LandUse"]], "CO2LandUse"
  )
  q <- toolMagpieVariableFrame(
    h12Long, .toolMagpieContractVars[["QRequested"]], "QRequested"
  )

  if (!setequal(unique(target$op_region), .toolMagpieOP39)) {
    stop(
      "toolBmswasLoadLandCO2AnchorsH12MAgPIE: expected exactly OP39 targets; got ",
      paste(sort(unique(target$op_region)), collapse = ", ")
    )
  }
  if (!setequal(unique(q$op_region), .toolMagpieH12)) {
    stop(
      "toolBmswasLoadLandCO2AnchorsH12MAgPIE: expected exactly H12 requested Q; got ",
      paste(sort(unique(q$op_region)), collapse = ", ")
    )
  }
  if (!setequal(unique(target$GHGScen), .toolMagpieScenarios) ||
      !setequal(unique(q$GHGScen), .toolMagpieScenarios)) {
    stop(
      "toolBmswasLoadLandCO2AnchorsH12MAgPIE: expected policy scenarios ",
      paste(.toolMagpieScenarios, collapse = ", ")
    )
  }

  target$h12_region <- ifelse(
    target$op_region %in% .toolMagpieEU28, "EUR", target$op_region
  )
  names(q)[names(q) == "op_region"] <- "h12_region"
  ids <- c("h12_region", "GHGScen", "Year", "BioDem")
  targetKey <- do.call(paste, c(target[c("op_region", ids)], sep = "\r"))
  qKey <- do.call(paste, c(q[ids], sep = "\r"))
  if (anyDuplicated(targetKey) || anyDuplicated(qKey)) {
    stop(
      "toolBmswasLoadLandCO2AnchorsH12MAgPIE: duplicate regression anchors"
    )
  }

  anchors <- merge(target, q, by = ids, all.x = TRUE, all.y = FALSE)
  anchors$Q <- anchors$QRequested / .toolMagpieEjPerMtoe
  if (nrow(anchors) != nrow(target) || any(!is.finite(anchors$Q)) ||
      any(anchors$Q < 0)) {
    stop(
      "toolBmswasLoadLandCO2AnchorsH12MAgPIE: every OP39 target must have ",
      "one finite, non-negative requested H12 quantity"
    )
  }
  if (any(!is.finite(anchors$CO2LandUse))) {
    stop(
      "toolBmswasLoadLandCO2AnchorsH12MAgPIE: land CO2 targets must be finite"
    )
  }

  counts <- aggregate(
    BioDem ~ op_region + GHGScen + Year,
    data = anchors,
    FUN = function(z) length(unique(z))
  )
  if (any(counts$BioDem != 23L)) {
    stop(
      "toolBmswasLoadLandCO2AnchorsH12MAgPIE: expected exactly 23 demand ",
      "anchors in every OP39/scenario/year fit cell"
    )
  }
  anchors
}

# Numerically stable signed quadratic OLS. With fewer than three distinct Q
# values it falls back to a signed linear or constant fit.
toolMagpieFitQuadratic <- function(Q, Y) {
  ok <- is.finite(Q) & is.finite(Y)
  Q <- Q[ok]
  Y <- Y[ok]
  if (!length(Q)) return(c(0, 0, 0))
  uq <- unique(Q)
  if (length(uq) == 1L) return(c(mean(Y), 0, 0))
  if (length(uq) == 2L) {
    co <- .toolLandUseEmulatorLstsq(cbind(1, Q), Y)
    return(c(co[1], co[2], 0))
  }

  center <- mean(Q)
  scale <- max(abs(Q - center))
  if (!is.finite(scale) || scale <= .Machine$double.eps) {
    return(c(mean(Y), 0, 0))
  }
  z <- (Q - center) / scale
  co <- .toolLandUseEmulatorLstsq(cbind(1, z, z^2), Y)
  c(
    co[1] - co[2] * center / scale + co[3] * center^2 / scale^2,
    co[2] / scale - 2 * co[3] * center / scale^2,
    co[3] / scale^2
  )
}

# Numerically stable signed linear OLS in the common three-coefficient schema.
toolMagpieFitLinear <- function(Q, Y) {
  ok <- is.finite(Q) & is.finite(Y)
  Q <- Q[ok]
  Y <- Y[ok]
  if (!length(Q)) return(c(0, 0, 0))
  if (length(unique(Q)) == 1L) return(c(mean(Y), 0, 0))
  co <- .toolLandUseEmulatorLstsq(cbind(1, Q), Y)
  c(co[1], co[2], 0)
}

# Fit a three-parameter function independently at each region, GHG scenario,
# and source year. fitf must return three coefficients. The returned diagnostic
# columns describe the source pivots and in-sample fit without changing the
# coefficient table consumed downstream.
toolMagpieFitCells <- function(anchors, target, fitf, degree = 2L) {
  if (!(degree %in% c(1L, 2L))) stop("toolMagpieFitCells: degree must be 1 or 2")
  cells <- unique(anchors[c("op_region", "GHGScen", "Year")])
  rows <- vector("list", nrow(cells))
  for (i in seq_len(nrow(cells))) {
    use <- anchors$op_region == cells$op_region[i] &
           anchors$GHGScen == cells$GHGScen[i] &
           anchors$Year == cells$Year[i]
    q <- anchors$Q[use]
    y <- anchors[[target]][use]
    ok <- is.finite(q) & is.finite(y)
    if (!any(ok)) {
      stop("toolMagpieFitCells: no finite anchors for ",
           paste(cells[i, ], collapse = "/"), " target=", target)
    }
    co <- fitf(q[ok], y[ok])
    if (length(co) != 3L || any(!is.finite(co))) {
      stop("toolMagpieFitCells: invalid fit for ",
           paste(cells[i, ], collapse = "/"), " target=", target)
    }
    design <- cbind(1, q[ok])
    if (degree == 2L) design <- cbind(design, q[ok]^2)
    rank <- qr(design)$rank
    prediction <- co[1] + co[2] * q[ok] + co[3] * q[ok]^2
    residual <- prediction - y[ok]
    denominator <- sum(abs(y[ok]))
    nsae <- if (denominator > .Machine$double.eps) {
      sum(abs(residual)) / denominator
    } else if (all(abs(residual) <= 1e-12)) {
      0
    } else {
      NA_real_
    }
    rows[[i]] <- data.frame(
      op_region = cells$op_region[i],
      ghgscen = cells$GHGScen[i],
      period = cells$Year[i],
      p1 = co[1], p2 = co[2], p3 = co[3],
      n = sum(ok),
      n_unique_q = length(unique(q[ok])),
      rank = rank,
      mae = mean(abs(residual)),
      max_ae = max(abs(residual)),
      nsae = nsae,
      fallback = rank < degree + 1L,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

# Interpolate one fitted coefficient from all source years to annual 2010..2100,
# holding the nearest endpoint outside the source range.
toolMagpieInterpAnnual <- function(year, value,
                                   outYears = .toolMagpieOutYears) {
  ok <- is.finite(year) & is.finite(value)
  year <- as.integer(year[ok])
  value <- value[ok]
  ord <- order(year)
  year <- year[ord]
  value <- value[ord]
  if (!length(year)) return(rep(0, length(outYears)))
  if (length(year) == 1L) return(rep(value, length(outYears)))
  approx(year, value, xout = outYears, rule = 2, ties = "ordered")$y
}

# Expand cell fits to annual rows in the common coefficient-table schema.
# coefNames controls the labels of p1/p2/p3.
toolMagpieAnnualCoefficientRows <- function(fits, coefNames) {
  keys <- unique(fits[c("op_region", "ghgscen")])
  rows <- list()
  for (i in seq_len(nrow(keys))) {
    z <- fits[fits$op_region == keys$op_region[i] &
              fits$ghgscen == keys$ghgscen[i], ]
    for (j in seq_along(coefNames)) {
      rows[[length(rows) + 1L]] <- data.frame(
        op_region = keys$op_region[i],
        ghgscen = keys$ghgscen[i],
        coef = coefNames[j],
        period = .toolMagpieOutYears,
        value = toolMagpieInterpAnnual(z$period, z[[paste0("p", j)]]),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}
