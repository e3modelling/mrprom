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

  list(x = x,
       weight = NULL,
       description = c(category = "Transportation",
                       type = "Transport Lifetimes ",
                       filename = "LFT_TRANSTECH.xlsx",
                       `Indicative size (MB)` = 0.014,
                       dimensions = "3D",
                       unit = "years",
                       Confidential = "open"))
}
