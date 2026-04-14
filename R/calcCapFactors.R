#' calcCapFactors
#' Calibrate average capacity factors for 2024 using EurostatHDD according to 2010 data from HotMaps
#' 
#'
#' @return magpie object with OPENPROM input data iCapFactors
#'
#' @author Margarita Efthymiadou, Fotis Sioutas
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

 a <- readSource("HotMaps")

 b <- readSource("EurostatHDD")

 qa <- as.magpie(a)
 qb <- as.magpie(b)

qb_2024 <- qb[, "y2024", ]
qb_2010 <- qb[, "y2010", ]

ratio <- qb_2024 / qb_2010
ratio[!is.finite(ratio)] <- NA

ratio <- dimSums(ratio, dim = "variable.unit")

x <- qa * ratio

  list(
    x = x,
    weight = NULL,
    unit = "%",
    description = "Average capacity factors for 2024"
  )
}
