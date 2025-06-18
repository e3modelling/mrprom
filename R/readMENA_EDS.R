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

readMENA_EDS <- function(subtype =  "VEH") {

  x <- suppressWarnings(readGDX(gdx = "fulldata.gdx", name = subtype, field = "l"))

  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "MENA_EDS gdx files",
                       type = "MENA_EDS gdx files",
                       filename = "fulldata.gdx",
                       `Indicative size (MB)` = 92,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "E3M"))
}
