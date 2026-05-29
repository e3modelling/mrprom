#' calcIDataPGScaleEndogScrap
#'
#' Use uncalibrated data to derive default values for iScaleEndogScrap
#' It includes parameters for premature scrapping in the PG module.
#'
#' @return magpie object with OPENPROM input data iScaleEndogScrap
#'
#' @author Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataPGScaleEndogScrap", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% mutate
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom madrat toolGetMapping
#' @importFrom tidyr crossing
calcIDataPGScaleEndogScrap <- function() {
  extdata <- readEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )

  SECTTECH <- toolGetMapping("PGALL.csv",
    type = "blabla_export",
    where = "mrprom"
  )

  regions <- unname(getISOlist())

  data <- crossing(
    SECTTECH,
    period = seq(extdata["fStartHorizon"], extdata["fEndHorizon"]),
    region = regions
  ) %>%
    mutate(value = 0.1) %>%
    as.quitte() %>%
    as.magpie()

  list(
    x = data,
    weight = data,
    unit = "(1)",
    description = "Maturty factors on PG Premature scrapping"
  )
}
