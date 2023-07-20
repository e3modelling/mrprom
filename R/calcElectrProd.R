#' calcElectrProd
#'
#' @return Read ENERDATA data and convert it to a csvr file
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "ElectrProd", file = "iElectrProd.csvr")
#' }
#'
#' @importFrom quitte as.quitte interpolate_missing_periods


calcElectrProd <- function() {

  x <- readSource("ENERDATA", "electricity production",convert = TRUE)
  x <- toolCountryFill(x)
  x[is.na(x)] <- 0
  x <- as.quitte(x)
  x <- interpolate_missing_periods(x, period = (seq(2010, 2100, 1)))
  x <- as.magpie(x)
  u <- getItems(x, "unit")
  getNames(x) <- sub("\\..*$", "", getNames(x)) # remove units in the file read by GAMS

  return(list(x = collapseNames(x),
              weight = NULL,
              unit = u,
              description = "ENERDATA; electricity production"))
}
