#' calcDAC
#'
#' @return The DAC
#'
#' @author Dionysis P
#'
#' @examples
#' \dontrun{
#' x <- calcOutput("DAC", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr %>% filter

calcDAC <- function() {
  
  x <- readSource("DAC")
  
  list(x = x,
       weight = NULL,
       unit = "various",
       description = "")
}
