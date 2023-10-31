#' calcPOP
#'
#' The SSP data filtered by pop
#'
#' @param scenario string. By choosing a scenario you filter the SSP dataset
#' by type.
#'
#' @return The SSP data filtered by pop
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' POP <- calcOutput("POP", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte interpolate_missing_periods

calcPOP <- function(scenario = 'SSP2') {

x <- readSource("SSP", "pop", convert = FALSE)/1000
getSets(x) <- c('region','period','model','scenario','variable','unit')
x <- toolCountryFill(x)
x[is.na(x)] <- 0
x <- as.quitte(x[, , scenario]) %>% interpolate_missing_periods(period = seq(2010, 2100, 1))

list(x = collapseNames(as.magpie(x)),
     weight = NULL,
     unit = "billion",
     description = "Population; Source: SSP Scenarios (IIASA)")
}