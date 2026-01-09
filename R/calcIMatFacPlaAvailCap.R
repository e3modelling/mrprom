#' calcIMatFacPlaAvailCap
#'
#' Use uncalibrated data to derive default values for iMatFacPlaAvailCap.
#' This dataset includes Maturity factors on PG Capacity.
#'
#' @return magpie object with OPENPROM input data iMatFacPlaAvailCap.
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IMatFacPlaAvailCap", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% mutate
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom madrat toolGetMapping
calcIMatFacPlaAvailCap <- function() {
  extdata <- readEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )
  techs <- toolGetMapping("PGALL.csv",
    type = "blabla_export",
    where = "mrprom"
  )

  data <- expand.grid(
    period = seq(extdata["fStartHorizon"], extdata["fEndHorizon"]),
    variable = techs$PGALL
  ) %>%
    mutate(value = 1) %>%
    as.quitte() %>%
    as.magpie()

  list(
    x = data,
    weight = NULL,
    unit = "(1)",
    description = "Maturty factors on Capacity"
  )
}
