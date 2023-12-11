#' calcIMinResPot
#'
#' The res min potential data calculated from res max potential data by
#' deviding the max by two, to derive OPENPROM input parameter iMinResPot.
#' The res max potential data is from the "EUROPEAN COMMISSION" and 
#' MENA_EDS model data.
#' 
#' @return  OPENPROM input data iMinResPot
#' The output data for EU countries calculated from the "EUROPEAN COMMISSION".
#' The output data for middle East and north Africa calculated from MENA_EDS
#' model. Countries with NA is used the region or global mean value.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IMinResPot", aggregate = FALSE)
#' }
#'

calcIMinResPot <- function() {
  
  x <- calcOutput(type = "IMaxResPot", aggregate = FALSE)
  x <- x / 2
  
  return(list(x = x,
              weight = NULL,
              unit = "GW",
              description = "EUROPEAN COMMISSION and MENA_EDS model"))
}