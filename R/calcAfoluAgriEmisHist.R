#' calcAfoluAgriEmisHist
#'
#' Produce the common 2010-2025 Agriculture CH4 and N2O history for the
#' MAgPIE and GLOBIOM land-use emulators. Values are linearly interpolated
#' between the four frozen native-MAgPIE OP39 anchors.
#'
#' @return list with \code{x}, a magclass object [region, year, emtype].
#' @author Songmin
#' @keywords internal
calcAfoluAgriEmisHist <- function() {
  x <- toolAfoluHistoryAnnual()[, , c("CH4LandUse", "N2OLandUse")]
  magclass::getSets(x) <- c("region", "year", "emtype")
  list(
    x = x,
    weight = NULL,
    isocountries = FALSE,
    unit = "CH4 Mt/yr; N2O kt/yr",
    description = paste(
      "Common MAgPIE/GLOBIOM-emulator historical agriculture CH4/N2O;",
      "N2O includes direct and indirect agricultural nitrogen-cycle emissions"
    )
  )
}
