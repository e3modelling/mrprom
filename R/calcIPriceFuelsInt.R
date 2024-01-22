#' calcIPriceFuelsInt
#'
#' Use data from ENERDATA to derive OPENPROM input parameter iPriceFuelsInt
#' This dataset includes fossil fuel prices from the IEA World Energy Outlook, in $2015/toe.
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
#' @importFrom tibble deframe
#' @importFrom utils tail
 
calcIPriceFuelsInt <- function() {

  x <- readSource("IEA_WEO", subtype = "FuelPrices")
  
  # Read the WEF set including fossil fuels
  promnames <- readSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "WEF")
  promnames <- unlist(strsplit(promnames[, 1], ","))
  
  
  # Adding the PROM variables with placeholder values
  for (name in promnames) {
    x <- add_columns(x, addnm = name, dim = "variable", fill = 0.00000001)
  }

  # Setting the IEA variables
  x[, , 'WCRO'] <- x[, ,'IEA crude oil']
  
  
  list(x = x,
       weight = NULL,
       unit = "$2015/toe",
       description = "Enerdata; Fuel Exports")
}
