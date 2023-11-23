#' readMENA_EDS
#'
#' Read MENA_EDS gdx files, convert it to a MENA_EDS mif file so to compare output mif file
#' with OPEN-PROM output.
#' 
#' @param subtype Variable of MENA_EDS
#'
#' @return The read-in data into a mif object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("MENA_EDS", subtype =  "VEH")
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom dplyr select
#' @importFrom tidyr unite
#' @importFrom quitte as.quitte write.mif

readMENA_EDS <- function(subtype) {

  x <- suppressWarnings(readGDX(gdx = "fulldata.gdx", name = subtype, field = "l"))
  
  return(as.magpie(x))
}
