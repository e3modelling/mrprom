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
#' @importFrom dplyr filter %>%

calcPOP <- function(scenario = "SSP2") {

  x <- readSource("SSP", "pop", convert = FALSE) / 1000 # convert millions to billions
  x <- x[,,"IIASA-WiC POP 2023"][,,scenario][,,"Population"]
  period <- NULL
  x <- as.quitte(x) %>% interpolate_missing_periods(period = seq(2010, 2100, 1))
  x[["region"]] <- toolCountry2isocode(x[["region"]])
  x <- filter(x, !is.na(x[["region"]]))
  x <- filter(x, period %in% c(2010 : 2100))
  x <- as.quitte(x) %>% as.magpie()
  x <- toolCountryFill(x)
  x[is.na(x)] <- 0

  list(x = collapseNames(as.magpie(x)),
       weight = NULL,
       unit = "billion",
       description = "Population; Source: SSP Scenarios (IIASA)")
}
