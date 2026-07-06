#' calcIPriceTransElast
#'
#' Fuel price pass-through elasticity table for OPEN-PROM's unified price
#' transmission (module 08 Q08). One row per (source fuel, target fuel) pair;
#' the value is the elasticity e in the year-over-year factor
#' (P_source(t)/P_source(t-1))^e that multiplies the target fuel's price.
#'
#' Source values are hand-maintained in the PROMParameters madrat source folder:
#'   CRO rows    reproduce the legacy inline crude-oil transmission (0.4/0.8/0.2);
#'   BMSWAS rows are the biomass -> bioenergy-form transmission (0.6, placeholder,
#'               to be calibrated).
#'
#' @return magpie object with OPEN-PROM input data iPriceTransElast (GLO x source.target)
#'
#' @author Songmin Yu
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IPriceTransElast", aggregate = FALSE)
#' }
#'
calcIPriceTransElast <- function() {

  x <- readSource("PROMParameters", subtype = "PriceTransElast")

  list(x = x,
       weight = NULL,
       unit = "(1)",
       description = "Fuel price pass-through elasticity, source fuel -> target fuel (Q08 unified price transmission)")
}
