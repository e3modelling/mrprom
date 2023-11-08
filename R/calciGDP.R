#' calciGDP
#'
#' The SSP data filtered by gdp
#'
#' @param scenario string. By choosing a scenario you filter the SSP dataset
#' by type.
#'
#' @return The SSP data filtered by gdp
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' gdp <- calcOutput("iGDP", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte interpolate_missing_periods

calciGDP <- function(scenario = "SSP2") {


  x <- readSource("SSP", "gdp", convert = TRUE)
  getSets(x) <- c("region", "period", "model", "scenario", "variable", "unit")
  x <- as.quitte(x[, , scenario]) %>% interpolate_missing_periods(period = seq(2010, 2100, 1))
  x["value"] <- x["value"] * (1.2136)
  list(x = collapseNames(as.magpie(x)),
       weight = NULL,
       unit = "billion US$2015/yr",
       description = "GDP|PPP; Source: SSP Scenarios (IIASA)")
}
