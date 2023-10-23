calcPOP <- function(scenario = 'SSP2') {

library("dplyr")
library("tidyr")
library("magclass")
library("quitte")

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