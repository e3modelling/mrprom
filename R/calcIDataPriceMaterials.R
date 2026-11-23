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
  
  # filter years
  fStartHorizon <- toolReadEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fEndY <- toolReadEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndY"]
  
  SCRAP <- 0.432
  IRONORE <- 0.199
  
  countries <- as.character(getISOlist())
  
  x <- expand.grid(
    region = countries,
    period = seq(fStartHorizon, fEndY),
    variable = c("SCRAP", "IRONORE")
  ) %>%
    mutate(
      value = case_when(
        variable == "SCRAP" ~ SCRAP,
        variable == "IRONORE" ~ IRONORE
      ),
      unit = "kUS$2015/tn"
    ) %>%
    select(region, variable, unit, period, value) %>%
    as.quitte()
  
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       unit = "kUS$2015/tn",
       description = "IDataPriceMaterials")
  
}
