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

  all <- suppressWarnings(readGDX(gdx = "fulldata.gdx", name = subtype, types = "variables", field = "l"))

  x <- as.quitte(all)
  x["model"] <- "MENA_EDS"
  x["variable"] <- subtype
  cols1 <- names(x)[!names(x) %in% c("ytime", "allcy")]
  cols2 <- names(x)[!names(x) %in% c("model", "scenario", "region", "unit", "period", "value", "ytime", "allcy")]
  x <- select(x, all_of(cols1)) %>% unite(col = "variable", sep = " ", all_of(cols2))
  
  return(as.magpie(x))
}
