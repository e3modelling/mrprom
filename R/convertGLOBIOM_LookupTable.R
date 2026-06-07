#' convertGLOBIOM_LookupTable
#'
#' Pass-through. The GLOBIOM lookup is on GLOBIOM regions (not ISO countries),
#' so the usual ISO country-fill does not apply: the cross-region aggregation
#' to OPEN-PROM's region set happens in \code{\link{calcBmswasSupplyCoefGLOBIOM}} /
#' \code{\link{calcBmswasEmisCoefGLOBIOM}} via the bundled
#' globiom->openprom region mapping. Callers should use
#' \code{readSource("GLOBIOM_LookupTable", convert = FALSE)}; this stub exists
#' so a plain \code{readSource(...)} does not error.
#'
#' @param x magclass object from \code{\link{readGLOBIOM_LookupTable}}
#' @return \code{x} unchanged
#' @author Songmin
#' @examples
#' \dontrun{
#' a <- readSource("GLOBIOM_LookupTable")
#' }
#' @export
convertGLOBIOM_LookupTable <- function(x) {
  x
}
