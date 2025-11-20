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
  x1 <- x1[,,"OECD ENV-Growth 2025.Historical Reference.GDP|PPP.billion USD_2015/yr"]
  x1 <- collapseDim(x1, 3)
  x1 <- as.quitte(x1) %>% interpolate_missing_periods(period = seq(2010, 2025, 1), expand.values = TRUE)
  x1["variable"] <- scenario
  
  x2 <- readSource("SSP", "gdp", convert = TRUE) / 1000 # to billion
  x2 <- as.quitte(x2[, , scenario]) %>% interpolate_missing_periods(period = seq(2025, 2100, 1), expand.values = TRUE)
  x2["value"] <- x2["value"] * (0.97) # convert US$2017 to 2015
  
  x1 <- filter(x1, period %in% c(2010 : 2024))
  
  x2 <- filter(x2, period %in% c(2025 : 2100))
  x <- rbind(x1, x2)
  x[["unit"]] <- "GDP|PPP.billion US$2015/yr"
  x <- as.quitte(x) %>% as.magpie()
  x[is.na(x)] <- 0
  
  list(x = x,
       weight = NULL,
       unit = "billion US$2015/yr",
       description = "GDP|PPP; Source: SSP Scenarios (IIASA)")
}
