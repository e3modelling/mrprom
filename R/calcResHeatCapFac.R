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

  # --- calculate capacity factors ---  
  b_2024 <- b[, "y2024", ]
  b_2010 <- b[, "y2010", ]

  ratio <- b_2024 / b_2010
  ratio[!is.finite(ratio)] <- NA

  ratio <- collapseDim(ratio,2)
  ratio <- collapseDim(ratio,3)
  x <- a * ratio 
 x[is.na(x)] <- 1  #x=1 when NA values

  # --- clean data ---  
  xq <- as.quitte(x)  %>%
  mutate(variable = "res_heat_cap_factor")

  list(
    x = xq,
    weight = NULL,
    unit = "%",
    description = "Average residential heating capacity factors for 2024"
  )
}
