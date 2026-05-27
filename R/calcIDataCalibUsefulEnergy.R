#' calcIDataCalibUsefulEnergy
#'
#' Use uncalibrated data to derive default values for iCalibUsefulEnergy
#' It includes calibration parameters for useful energy in the industry module.
#'
#' @return magpie object with OPENPROM input data iCalibUsefulEnergy
#'
#' @author Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataCalibUsefulEnergy", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% mutate
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom madrat toolGetMapping
#' @importFrom tidyr crossing
calcIDataCalibUsefulEnergy <- function() {
  extdata <- readEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )

  DSBS <- toolGetMapping("DSBS.csv",
    type = "blabla_export",
    where = "mrprom"
  ) %>%
    select(DSBS)
  regions <- unname(getISOlist())

  data <- crossing(
    DSBS,
    period = seq(extdata["fStartHorizon"], extdata["fEndHorizon"]),
    region = regions
  ) %>%
    mutate(value = 0) %>%
    as.quitte() %>%
    as.magpie()

  list(
    x = data,
    weight = data,
    unit = "(1)",
    description = "Calibration parameters for useful energy"
  )
}
