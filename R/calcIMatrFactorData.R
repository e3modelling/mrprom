#' calcIMatrFactorData
#'

#' Derive default values for iMatFacPlaAvailCap using uncalibrated data.
#' Provides maturity factors for all technologies across demand subsectors.
#'
#' @return magpie object with OPENPROM input data iMatrFactorData.
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IMatrFactorData", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% mutate
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom madrat toolGetMapping
calcIMatrFactorData <- function() {
  extdata <- readEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )
  INDDOM <- toolGetMapping("INDDOM.csv",
    type = "blabla_export",
    where = "mrprom"
  )
  NENSE <- toolGetMapping("NENSE.csv",
    type = "blabla_export",
    where = "mrprom"
  )
  TRANSE <- toolGetMapping("TRANSE.csv",
    type = "blabla_export",
    where = "mrprom"
  )
  SECTTECH <- toolGetMapping("SECTTECH.csv",
    type = "blabla_export",
    where = "mrprom"
  )

  data <- crossing(
    period = seq(extdata["fStartHorizon"], extdata["fEndHorizon"]),
    SECTTECH
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
