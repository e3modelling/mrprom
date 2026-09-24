# Utilities shared only where the GLOBIOM and MAgPIE emulator pipelines use
# exactly the same numerical or output-schema operation. Source-specific data
# loading, regional treatment, regression forms, and interpolation remain in
# their respective toolBmswasCurveFit<Source>.R files.

# Minimum-norm least squares via SVD. This tolerates rank-deficient design
# matrices and is used unchanged by both emulator pipelines.
.toolLandUseEmulatorLstsq <- function(A, Y) {
  s <- svd(A)
  tol <- max(dim(A)) * .Machine$double.eps * max(s$d)
  dinv <- ifelse(s$d > tol, 1 / s$d, 0)
  as.numeric(s$v %*% (dinv * crossprod(s$u, Y)))
}

# Convert a long coefficient table to the common magclass output schema.
.toolLandUseEmulatorCoefToMagpie <- function(df, keyOrder) {
  cols <- c("op_region", "period", keyOrder, "value")
  x <- as.magpie(
    df[cols], spatial = 1, temporal = 2, datacol = length(cols)
  )
  getSets(x) <- c("region", "year", keyOrder)
  x
}
