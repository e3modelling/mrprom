#' calcConstantPrice
#'
#' Read in several excel files with data from ENERDATA.
#'
#' The two available ENERDATA datasets are combined into one,
#' NA values are replaced with zeros and historical data are corrected.
#'
#' Finally, the combined ENERDATA datasets are filtered by constant price.
#'
#' @return The "ENERDATA" data filtered by constant price.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "ConstantPrice", file = "ConstantPrice.csv" , aggregate = FALSE)
#' }
#'

calcConstantPrice <- function() {

  x <- readSource("ENERDATA", "constant price", convert = TRUE)
  x[is.na(x)] <- 0
  u <- getItems(x, "unit")
  getNames(x) <- sub("\\..*$", "", getNames(x)) # remove units in the file read by GAMS

  list(x = collapseNames(x),
       weight = NULL,
       unit = u,
       description = "ENERDATA; electricity production")
}
