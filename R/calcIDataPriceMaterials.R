#' calcIDataPriceMaterials
#'
#' @return Price Materials
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataPriceMaterials", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% mutate case_when select
#' @importFrom quitte as.quitte
#'

calcIDataPriceMaterials <- function() {
  fStartHorizon <- toolReadEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fEndY <- toolReadEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndY"]


  x <- expand.grid(
    region = unname(getISOlist()),
    period = seq(fStartHorizon, fEndY),
    variable = c("SCRAP", "IRONORE")
  ) %>%
    mutate(
      value = case_when(
        variable == "SCRAP" ~ 0.432,
        variable == "IRONORE" ~ 0.199
      )
    ) %>%
    select(region, variable, period, value) %>%
    as.quitte() %>%
    as.magpie()

  list(
    x = x,
    weight = NULL,
    unit = "kUS$2015/tn",
    description = "IDataPriceMaterials"
  )
}
