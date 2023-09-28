#' calcINewREg
#'
#' Calculate the difference between one year of passenger-cars-in-use
#'
#' @return  Magpie object with the difference between one year of passenger-cars-in-use
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "INewREg", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom quitte as.quitte


calcINewREg <- function() {
  
  x <- readSource("IRF", "passenger-cars-in-use", convert = TRUE)
  qx <- as.quitte(x)

  y <- diff(as.numeric(unlist(qx["value"])), lag = nrow(unique(qx["region"])))
  y <- as.data.frame(y)
  
  z <- matrix(0, nrow(y), length(qx))
  z <- as.data.frame(z)
  z[, 1:6] <- qx[(nrow(unique(qx["region"]))+1):nrow(qx), 1:6]
  z[, 7] <- y
  names(z) <- names(qx)
  
  x <- as.quitte(z) %>% as.magpie()
  
  list(x = x,
       weight = NULL,
       unit = "vehicles",
       description = "IRF; passenger-cars-in-use per year")
  
}