#' calcIDataPremScrpFac
#'
#' Use uncalibrated data to derive default values for iPremScrpFac
#' This dataset includes Maturity factors for premature scrapping .
#'
#' @return magpie object with OPENPROM input data iPremScrpFac
#'
#' @author Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataPremScrpFac", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% mutate
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom madrat toolGetMapping
#' @importFrom tidyr crossing
calcIDataPremScrpFac <- function() {
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
    mutate(value = 0.1) %>%
    as.quitte() %>%
    as.magpie()

  list(
    x = data,
    weight = data,
    unit = "(1)",
    description = "Maturty factors on Premature scrapping"
  )
}
