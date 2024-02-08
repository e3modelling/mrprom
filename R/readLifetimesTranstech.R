#' readLifetimesTranstech
#'
#' Read in lifetimes for Transport sector
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("LifetimesTranstech")
#' }
#'
#' @importFrom readxl read_excel

readLifetimesTranstech <- function() {

  x <- read_excel("LFT_TRANSTECH.xlsx",
                   sheet = "LFT", range = "A1:C53")
  x <- as.quitte(x)
  x <- as.magpie(x)

  return(x)
}
