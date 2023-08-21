#' fullOPEN-PROM
#'
#' Read in several files with data from GEME3, IRF and WDI_PA and convert it
#' to a csv file.
#' The dataset contains several data types about transport, traffic, air
#' transport passengers per country and per year and Production Level and
#' Unit Cost data.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#'  a <- retrieveData("OPEN_PROM", regionmapping = "regionmappingOPDEV2.csv")
#' }

fullOPEN_PROM <- function() {

    x <- calcOutput(type = "ACTV", file = "iopen_prom.csv", aggregate = TRUE)

  return(list(x = x,
              weight = NULL,
              unit = "various",
              description = "readGEME3; Production Level and Unit Cost"))

}

