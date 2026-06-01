#' calciResHeatCapFac
#' 
#' Derives country-level residential heating capacity factors for 2024
#' by scaling baseline capacity factors from the HotMaps dataset using
#' changes in heating demand conditions from Eurostat Heating Degree Days
#' (HDD). The calibration is performed by applying the ratio of HDD in
#' 2024 relative to 2010 to the corresponding HotMaps capacity factors,
#' thereby adjusting historical heating utilization levels to reflect
#' recent climatic conditions.
#'
#' Malta is assumed to have the same residential heating capacity factor
#' as Cyprus due to missing HotMaps data. Where no calibrated value can
#' be derived, the capacity factor is set to 1.
#'
#' GDP projections from the OPEN-PROM input dataset iGDP are used as
#' aggregation weights for regional aggregation and disaggregation.
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
  ratio["GBR", , ] <- ratio["NLD", , ]

  ratio <- collapseDim(ratio,2)
  ratio <- collapseDim(ratio,3)
  x <- a * ratio 
  x[is.na(x)] <- 1  #x=1 when NA values
  x <- collapseDim(x,3.3)
  getItems(x, 3.1) <- "res_heat_cap_factor"
  
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
