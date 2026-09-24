#' calcAfoluLandCO2Hist
#'
#' Produce the common 2010-2025 history of land-use-change CO2
#' emissions/removals, excluding indirect land CO2 and fire emissions, for the
#' MAgPIE and GLOBIOM land-use emulators. Values are linearly interpolated
#' between the four frozen native-MAgPIE OP39 anchors.
#'
#' @return list with \code{x}, a magclass object [region, year, emtype].
#' @author Songmin
#' @keywords internal
calcAfoluLandCO2Hist <- function() {
  x <- toolAfoluHistoryAnnual()[, , "CO2LandUse"]
  magclass::getSets(x) <- c("region", "year", "emtype")
  list(
    x = x,
    weight = NULL,
    isocountries = FALSE,
    unit = "Mt CO2/yr",
    description = paste(
      "Common MAgPIE/GLOBIOM-emulator historical land-use-change CO2",
      "emissions/removals, excluding indirect land CO2 and fire emissions"
    )
  )
}
