#' calcPOP
#'
#' Derive population data based on the SSP scenarios
#'
#' @param scenario SSP scenario choice
#' Available scenario are:
#' \itemize{
#' \item `SSP1`:
#' \item `SSP2`:
#' \item `SSP3`:
#' \item `SSP4`:
#' \item `SSP5`:
#' }
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

calcPOP <- function(scenario = "SSP2") {

x <- readSource("SSP", "pop", convert = FALSE) / 1000
getSets(x) <- c("region", "period", "model", "scenario", "variable", "unit")
x <- toolCountryFill(x)
x[is.na(x)] <- 0
x <- as.quitte(x[, , scenario]) %>% interpolate_missing_periods(period = seq(2010, 2100, 1))

list(x = collapseNames(as.magpie(x)),
     weight = NULL,
     unit = "billion",
     description = "Population; Source: SSP Scenarios (IIASA)")
}
