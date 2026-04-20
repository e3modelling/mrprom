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

  # --- assume MLT same as CYP in 'a' ---  
  if ("CYP" %in% dimnames(a)[[1]]) {
    a["MLT", , ] <- a["CYP", , ]
  }
  
  # --- calculate capacity factors ---  
  b_2024 <- b[, "y2024", ]
  b_2010 <- b[, "y2010", ]

  ratio <- b_2024 / b_2010

  ratio <- collapseDim(ratio,2)
  ratio <- collapseDim(ratio,3)
  x <- a * ratio 
  x[is.na(x)] <- 1  #x=1 when NA values
  x <- collapseDim(x,3.3)
  getItems(x, 3.1) <- "res_heat_cap_factor"

  # --- assume MLT same as CYP ---  
  
  # Calculation of aggregation weights
  GDP <- calcOutput("iGDP", aggregate = FALSE) # will use gdp as disaggregation weights
  weights <- x
  weights[, , ] <- GDP[,2023,]
  
  list(
    x = x,
    weight = weights,
    unit = "%",
    description = "Average residential heating capacity factors for 2024"
  )
}
