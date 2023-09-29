#' calcINewReg
#'
#' Calculate the passenger-car-first-registrations per year
#'
#' @return  Magpie object with the passenger-car-first-registrations per year
#' 
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "INewReg", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom quitte as.quitte


calcINewReg <- function() {
  
  x <- readSource("IRF", "passenger-car-first-registrations", convert = TRUE)
  #vehicles
  getNames(x) <- "passenger-car-first-registrations.million vehicles"
  x <- x/10^6
  #million vehicles
  
  list(x = x,
       weight = NULL,
       unit = "million vehicles",
       description = "IRF; passenger-car-first-registrations")
  
}