#' calcCapFactors
#' Calibrate average capacity factors for 2024 using EurostatHDD according to 2010 data from HotMaps
#' 
#'
#' @return magpie object with OPENPROM input data iCapFactors
#'
#' @author Margarita Efthymiadou
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "iCapFactors", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% mutate
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom madrat toolGetMapping
#' @importFrom tidyr crossing
calcCapFactors <- function() {


  list(
    x = data,
    weight = data,
    unit = "%",
    description = "Average capacity factors for 2024"
  )
}
