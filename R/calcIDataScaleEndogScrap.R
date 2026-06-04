#' calcIDataScaleEndogScrap
#'
#' Use uncalibrated data to derive default values for iScaleEndogScrap
#' Itincludes parameters for premature scrapping in the industry module.
#'
#' @return magpie object with OPENPROM input data iScaleEndogScrap
#'
#' @author Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataScaleEndogScrap", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% mutate
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom madrat toolGetMapping
#' @importFrom tidyr crossing
calcIDataScaleEndogScrap <- function() {
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
    description = "Maturty factors on Premature scrapping"
  )
}
