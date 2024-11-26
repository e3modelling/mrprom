#' calcIPriceFuelsInt
#'
#' Use data from IEA and ENERDATA to derive OPENPROM input parameter iPriceFuelsInt
#' This dataset includes fossil fuel prices from IEA World Energy Outlook and ENERDATA, in $2015/toe.
#'
#' @return magpie object with OPENPROM input data iPriceFuelsInt.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IPriceFuelsInt", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join case_when if_else arrange
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom utils tail

calcIPriceFuelsInt <- function() {

  x <- readSource("IEA_WEO", subtype = "FuelPrices")
  spot <- readSource("ENERDATA2", subtype = "spot", convert = FALSE)

  # Read the WEF set including fossil fuels
  promnames <- toolGetMapping(paste0("WEF.csv"),
                         type = "blabla_export",
                         where = "mrprom")
  
  promnames <- as.character(sets[, 1])

  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  lastYear <- tail(sort(getYears(spot, as.integer = TRUE)), 1)
  spot <- spot[, c(fStartHorizon:lastYear), ]

  # Converting dataframes to quitte objects and interpolating periods
  x <- as.quitte(x) %>%
    interpolate_missing_periods(period = c(fStartHorizon:2100), expand.values = TRUE) %>%
    as.magpie()

  spot <- as.quitte(spot) %>%
    interpolate_missing_periods(period = c(fStartHorizon:2100), expand.values = TRUE) %>%
    as.magpie()

  # Price conversion from $2021/barrel to $2015/toe
  spot["World", , "Spot price of gasoline in Rotterdam.us$/bbl"] <- spot["World", , "Spot price of gasoline in Rotterdam.us$/bbl"] * 0.8747 * 7.2
  spot["World", , "Spot price of heavy fuel oil in Rotterdam.us$/bbl"] <- spot["World", , "Spot price of heavy fuel oil in Rotterdam.us$/bbl"] * 0.8747 * 7.2

  # Adding the PROM variables with placeholder values
  for (name in promnames) {
    x <- add_columns(x, addnm = name, dim = "variable", fill = 0.00000001)
  }

  # Setting the PROM variables
  x[, , "WHCL"] <- x[, , "Steam coal average.$2015/toe"]
  x[, , "WCOKE"] <- x[, , "Steam coal average.$2015/toe"] * 1.245
  x[, , "WCRO"] <- x[, , "IEA crude oil.$2015/toe"]
  x[, , "WNGS"] <- x[, , "Natural gas average.$2015/toe"]
  x[, , "WGSL"] <- spot["World", , "Spot price of gasoline in Rotterdam"]
  x[, , "WGDO"] <- spot["World", , "Spot price of heavy fuel oil in Rotterdam"] * 1.07
  x[, , "WRFO"] <- spot["World", , "Spot price of heavy fuel oil in Rotterdam"]

  # Only keeping the PROM variables
  x <- x[, , promnames]

  # Creating projections for lignite prices
  x[, 2010, "WLGN"] <- 1306 # setting a base price for LGN

  time_range <- c(2011:2050)
  for (i in time_range) {
      curr <- i
      prev <- i - 1
      x[, curr, "WLGN"] <- x[, curr, "WCOKE"] - x[, prev, "WCOKE"] + x[, prev, "WLGN"]
  }

  # Creating projections for the spot prices of gasoline, diesel and heavy fuel oil
  time_range <- c(2022:2050)
  for (i in time_range) {
    curr <- i
    prev <- i - 1

    # Calculating the oil price ratio for each year
    oil_price_ratio <- x[, curr, "WCRO"] / x[, prev, "WCRO"]

    # Calculating the projection by multiplying the previous price by the oil price ratio
    x[, curr, "WGSL"] <- x[, prev, "WGSL"] * oil_price_ratio
    x[, curr, "WGDO"] <- x[, prev, "WGSL"] * oil_price_ratio
    x[, curr, "WRFO"] <- x[, prev, "WRFO"] * oil_price_ratio
  }

  list(x = x,
       weight = NULL,
       unit = "$2015/toe",
       description = "IEA and Enerdata; Fossil Fuel Prices")

}
