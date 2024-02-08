#' calcIPriceFuelsIntBase
#'
#' Use data from IEA and ENERDATA to derive OPENPROM input parameter iPriceFuelsIntBase
#' This dataset includes fossil fuel prices from IEA World Energy Outlook 2023, in $2015/toe.
#'
#' @return magpie object with OPENPROM input data iPriceFuelsIntBase.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IPriceFuelsIntBase", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join case_when if_else arrange
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom utils tail

calcIPriceFuelsIntBase <- function() {

  x <- calcOutput(type = "IPriceFuelsInt", aggregate = FALSE)

  # Only keeping prices for crude oil and natural gas
  fuels <- c("WCRO", "WNGS")
  x <- x[, , fuels]

  list(x = x,
       weight = NULL,
       unit = "$2015/toe",
       description = "IEA; Fossil Fuel Prices")
}
