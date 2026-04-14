#' calcCapFactors
#' Calibrate average residential heating capacity factors for 2024 using EurostatHDD according to 2010 data from HotMaps
#' 
#'
#' @return magpie object with OPENPROM input data iResHeatCapFac
#'
#' @author Margarita Efthymiadou, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "iResHeatCapFac", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% mutate
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom madrat toolGetMapping
#' @importFrom tidyr crossing
calciResHeatCapFac <- function() {

  # --- read data ---
  a <- readSource("HotMaps")
  b <- readSource("EurostatHDD")

  qa <- as.magpie(a)
  qb <- as.magpie(b)

  # --- calculate capacity factors ---  
  qb_2024 <- qb[, "y2024", ]
  qb_2010 <- qb[, "y2010", ]

  ratio <- qb_2024 / qb_2010
  ratio[!is.finite(ratio)] <- NA

  x <- qa * ratio %>%
    dimSums(dim = setdiff(getSets(x), "region"))
  
  # --- clean data ---  
  xq <- as.quitte(x) 
  xq <- xq[, !(names(xq) %in% c("variable1", "unit1"))] %>%
  mutate(variable = "res_heat_cap_factor")

  list(
    x = xq,
    weight = NULL,
    unit = "%",
    description = "Average residential heating capacity factors for 2024"
  )
}
