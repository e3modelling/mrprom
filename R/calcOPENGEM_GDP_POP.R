#' calcOPENGEM_GDP_POP
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
#' gdp <- calcOutput("OPENGEM_GDP_POP", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr %>% filter

calcOPENGEM_GDP_POP <- function(scenario = "SSP2") {
  
  OECD_SSP_GDP <- readSource("OECD_SSP_GDP", convert = TRUE)
  OECD_SSP_GDP <- as.quitte(OECD_SSP_GDP) %>%
    interpolate_missing_periods(period = 2010 : 2100, expand.values = TRUE) %>%
    as.magpie()
  getItems(OECD_SSP_GDP, 3.2) <- "GDP|PPP_billion US$2015/yr"
  getItems(OECD_SSP_GDP, 3.1) <- scenario
  OECD_SSP_GDP[is.na(OECD_SSP_GDP)] <- 0
  
  OPEN_GEM <- readSource("OPEN_GEM", convert = FALSE, subset = "GDP_POP")
  
  list(x = OECD_SSP_GDP,
       weight = NULL,
       unit = "various",
       description = "GDP|PPP; POP 2025 OPEN_GEM")
}
