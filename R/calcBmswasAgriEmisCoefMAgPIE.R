#' calcBmswasAgriEmisCoefMAgPIE
#'
#' Fit MAgPIE agriculture CH4 and N2O coefficients
#' \code{E = ea + eb * Q + ec * Q^2} independently for every carbon-policy
#' scenario, OPEN-PROM region, and lookup year. Q is actual second-generation
#' biomass use in Mtoe/yr. OPEN-PROM evaluates these curves as
#' \code{max(0, E)} so agriculture emissions cannot become negative.
#'
#' Coefficients are linearly interpolated to annual 2010..2100 and written by
#' \code{fullOPEN-PROM} to \code{iBmswasAgriEmisCoef_magpie.csv}, loaded by
#' GAMS as \code{i08AgriEmisCoefMagpie}.
#'
#' @return list(x = magclass [region, year, ghgscen.emtype.ecoef], ...)
#' @author Songmin
#' @examples
#' \dontrun{
#' x <- calcOutput("BmswasAgriEmisCoefMAgPIE", aggregate = FALSE)
#' }
#' @seealso \code{\link{calcBmswasLandEmisCoefMAgPIE}},
#'   \code{\link{calcBmswasBioPriceH12MAgPIE}}
#' @export
calcBmswasAgriEmisCoefMAgPIE <- function() {
  anchors <- toolBmswasLoadAnchorsMAgPIE()
  rows <- list()
  for (emtype in c("CH4LandUse", "N2OLandUse")) {
    fits <- toolMagpieFitCells(
      anchors,
      target = emtype,
      fitf = toolMagpieFitQuadratic
    )
    z <- toolMagpieAnnualCoefficientRows(fits, c("ea", "eb", "ec"))
    z$emtype <- emtype
    rows[[length(rows) + 1L]] <- z
  }
  df <- do.call(rbind, rows)
  x <- .toolLandUseEmulatorCoefToMagpie(
    df,
    keyOrder = c("ghgscen", "emtype", "coef")
  )
  getSets(x) <- c("region", "year", "ghgscen", "emtype", "ecoef")

  list(
    x = x,
    weight = NULL,
    isocountries = FALSE,
    unit = "CH4: Mt/yr; N2O: kt/yr; Q: Mtoe/yr",
    description = paste(
      "MAgPIE agriculture CH4/N2O curves E=ea+eb*Q+ec*Q^2;",
      "OPEN-PROM clamps evaluated emissions at zero"
    )
  )
}
