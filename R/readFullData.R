#' readFullData
#' 
#' Read MENA_EDS FullData gdx file
#' 
#' @param subtype Variable of MENA_EDS
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("FullData", subtype =  "VEH")
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom quitte as.quitte

readFullData <- function(subtype) {
  
  x <- NULL
  x <- readGDX(gdx = "fulldata.gdx", name = subtype, field = "l")
  
  return(x)
}
