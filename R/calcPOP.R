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

  x <- readSource("SSP", "pop", convert = TRUE) / 1000 # convert millions to billions
  pop <- readSource("SSPold")
  x1 <- pop[,,"IIASA-WiC POP 2025.Historical Reference.Population.million"] / 1000 # convert millions to billions
  x2 <- x[,,scenario]
  x1 <- collapseDim(x1, 3)
  x2 <- collapseDim(x2, 3)
  period <- NULL
  x1 <- as.quitte(x1) %>% interpolate_missing_periods(period = seq(2010, 2025, 1), expand.values = TRUE)
  x2 <- as.quitte(x2) %>% interpolate_missing_periods(period = seq(2025, 2100, 1), expand.values = TRUE)
  x1 <- filter(x1, !is.na(x1[["region"]]))
  x2 <- filter(x2, !is.na(x2[["region"]]))
  x1 <- filter(x1, period %in% c(2010 : 2024))
  x2 <- filter(x2, period %in% c(2025 : 2100))
  x <- rbind(x1, x2)
  x[["unit"]] <- "billions"
  x[["variable"]] <- scenario
  x <- as.quitte(x) %>% as.magpie()
  x <- toolCountryFill(x)
  x[is.na(x)] <- 0

  list(x = x,
       weight = NULL,
       unit = "billion",
       description = "Population; Source: SSP Scenarios (IIASA)")
}
