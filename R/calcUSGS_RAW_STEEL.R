#' calcUSGS_RAW_STEEL
#'
#' Derive USGS_RAW_STEEL in Mt
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "USGS_RAW_STEEL", aggregate = TRUE)
#' }
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr filter select

calcUSGS_RAW_STEEL <- function() {
  
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  x <- readSource("USGS", convert = TRUE)
  
  x <- as.quitte(x) %>%
    interpolate_missing_periods(period = fStartHorizon : sub("y", "", max(sort(getYears(x)), 1)), expand.values = TRUE)
  
  x <- as.quitte(x) %>% as.magpie()
  
  # set NA to 0
  x[is.na(x)] <- 10^-6
  
  list(x = x,
       weight = NULL,
       unit = "Mt",
       description = "Mt IS")
}
