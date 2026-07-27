#' calcBmswasLandEmisCoefMAgPIE
#'
#' Fit signed MAgPIE land-use CO2 coefficients independently for every
#' carbon-policy scenario, OPEN-PROM region, and lookup year. The default form
#' is \code{E = ea + eb * Q}, represented in the common three-coefficient
#' schema with \code{ec = 0}; \code{form = "quadratic"} fits all three terms.
#' Q is the requested H12
#' second-generation biomass demand in Mtoe/yr: EU28 countries use the common
#' EUR total and the other eleven regions map one-to-one. Negative E is retained
#' because the source variable is net land CO2 emissions and can represent net
#' removals.
#'
#' Coefficients are linearly interpolated to annual 2010..2100 and written by
#' \code{fullOPEN-PROM} to \code{iBmswasLandEmisCoef_magpie.csv}, loaded by
#' GAMS as \code{i08LandCO2CoefMagpie}.
#'
#' @param form Regression form. \code{"linear"} is the production default and
#'   sets \code{ec} exactly to zero; \code{"quadratic"} fits all three
#'   coefficients while preserving the same output schema.
#' @return list(x = magclass [region, year, ghgscen.emtype.ecoef], ...)
#' @author Songmin
#' @examples
#' \dontrun{
#' x <- calcOutput("BmswasLandEmisCoefMAgPIE", aggregate = FALSE)
#' }
#' @seealso \code{\link{calcBmswasAgriEmisCoefMAgPIE}},
#'   \code{\link{calcBmswasBioPriceH12MAgPIE}}
#' @export
calcBmswasLandEmisCoefMAgPIE <- function(form = c("linear", "quadratic")) {
  form <- match.arg(form)
  anchors <- toolBmswasLoadLandCO2AnchorsH12MAgPIE()
  fitf <- switch(
    form,
    quadratic = toolMagpieFitQuadratic,
    linear = toolMagpieFitLinear
  )
  fits <- toolMagpieFitCells(
    anchors,
    target = "CO2LandUse",
    fitf = fitf
  )
  df <- toolMagpieAnnualCoefficientRows(fits, c("ea", "eb", "ec"))
  df$emtype <- "CO2LandUse"
  x <- .toolLandUseEmulatorCoefToMagpie(
    df,
    keyOrder = c("ghgscen", "emtype", "coef")
  )
  getSets(x) <- c("region", "year", "ghgscen", "emtype", "ecoef")

  list(
    x = x,
    weight = NULL,
    isocountries = FALSE,
    unit = "E: Mt CO2/yr; Q: Mtoe/yr",
    description = paste(
      "MAgPIE net land CO2 curve fitted against requested H12 demand",
      paste0("(", form, "; signed)")
    )
  )
}
