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
#' @importFrom dplyr %>% filter

calciGDP <- function(scenario = "SSP2") {

  x1 <- readSource("SSPold")
  x1 <- x1[,,"SSP2.GDP|PPP.billion US$2005/yr"]
  x1 <- collapseDim(x1, 3)
  x1 <- as.quitte(x1) %>% interpolate_missing_periods(period = seq(2010, 2020, 1), expand.values = TRUE)
  x1["value"] <- x1["value"] * (1.2136) # convert US$2005 to 2015
  x1["variable"] <- "SSP2"
  
  x2 <- readSource("SSP", "gdp", convert = TRUE) / 1000 # to billion
  x2 <- as.quitte(x2[, , scenario]) %>% interpolate_missing_periods(period = seq(2020, 2100, 1), expand.values = TRUE)
  x2["value"] <- x2["value"] * (0.97) # convert US$2017 to 2015
  
  x1 <- filter(x1, period %in% c(2010 : 2019))
  
  x2 <- filter(x2, period %in% c(2020 : 2100))
  x <- rbind(x1, x2)
  x <- as.quitte(x) %>% as.magpie()
  x[is.na(x)] <- 0
  
  
  list(x = collapseNames(as.magpie(x)),
       weight = NULL,
       unit = "billion US$2015/yr",
       description = "GDP|PPP; Source: SSP Scenarios (IIASA)")
}
