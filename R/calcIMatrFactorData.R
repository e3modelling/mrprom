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
#' @importFrom tidyr crossing
calcIMatrFactorData <- function() {
  extdata <- readEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )
  SECTTECH <- toolGetMapping("SECTTECH.csv",
    type = "blabla_export",
    where = "mrprom"
  ) %>%
    separate_rows(c("TECH"), sep = ",") %>%
    separate_rows(c("DSBS"), sep = ",")
  regions <- unname(getISOlist())

  data <- crossing(
    SECTTECH,
    period = seq(extdata["fStartHorizon"], extdata["fEndHorizon"]),
    region = regions
  ) %>%
    mutate(value = 1) %>%
    as.quitte() %>%
    as.magpie()

  list(
    x = data,
    weight = data,
    unit = "(1)",
    description = "Maturity factors across demand subsectors"
  )
}
