#' calcPrimaryEnergyPrice
#' 
#' 
#' @return  OPENPROM input data PrimaryEnergyPrice
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "PrimaryEnergyPrice")
#' }
#'
#' @importFrom dplyr filter %>% mutate select last
#' @importFrom tidyr pivot_wider separate_rows crossing
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom eurostat get_eurostat


calcPrimaryEnergyPrice <- function() {
  
  EFS <- toolGetMapping(
    name = "EFS.csv",
    type = "blabla_export",
    where = "mrprom"
  ) %>%
    separate_rows(EFS, sep = ",")
  
  fStartHorizon <- toolReadEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fStartHorizon"]
  
  fEndY <- toolReadEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fEndY"]
  
  BMSWAS_Price <- readSource("MAgPIE_BMSWAS_Price")
  
  list(x = x,
       weight = w,
       unit = "",
       description = "Primary Energy Price")
  
}
