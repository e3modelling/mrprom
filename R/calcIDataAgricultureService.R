#' calcIDataAgricultureService
#'
#' @return  Magpie object with the FAOProductionCrops
#'
#' @author Fotis Sioutas
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataAgricultureService", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter left_join mutate select %>%
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom R.utils isZero

calcIDataAgricultureService <- function() {
  
  x <- readSource("FAOProductionCrops", convert = TRUE)
  x <- collapseDim(x, 3.2)
  x <- collapseDim(x, 3.1)
  x[is.na(x)] <- 0
  x <- x[,,c("ha","An","1000 An")]
  AreaHarvested <- x[,,c("Area harvested")]
  AreaHarvested <- AreaHarvested / 1000
  getItems(AreaHarvested, 3.2) <- "1000 ha"
  getItems(AreaHarvested, 3.1) <- "CROPS"
  Stocks <- x[,,c("1000 An")]
  Stocks2 <- x[,,c("An")]
  Stocks2 <- Stocks2 / 1000
  getItems(Stocks2, 3.2) <- "1000 An"
  Animal_stocks <- mbind(Stocks, Stocks2)
  getItems(Animal_stocks, 3.1) <- "LIVESTOCK"
  data <- mbind(AreaHarvested, Animal_stocks)
  data <- dimSums(data, 3.3)
  
  # complete incomplete time series
  qx <- as.quitte(data) %>%
    interpolate_missing_periods(period = getYears(data, as.integer = TRUE), expand.values = TRUE)
  
  x <- as.magpie(qx)
  
  list(
    x = x,
    weight = NULL,
    unit = "various",
    description = "FAOProductionCrops",
    mixed_aggregation = TRUE
  )
}
