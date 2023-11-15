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

  variable <- NULL
  all <- readGDX(gdx = "fulldata.gdx", name = subtype, types = "variables", field = "l")
  z <- c("ALG", "MOR", "TUN", "EGY", "ISR", "LEB", "JOR")
  years <- c(2017:2021)

  x <- as.quitte(all)
  x["model"] <- "MENA_EDS"
  x["variable"] <- subtype
  for (i in 8:length(x)) {
    if (x[1, i] != "NA"){
      x = unite(x, variable, c(variable, names(x)[i]), sep = " ", remove = FALSE)
    }
  }
  x <- select((x), -c(names(x)[8:length(x)]))
  x <- x[which(x$region %in% z), ]
  x <- x[which(x$period %in% years), ]
  x <- as.quitte(x)
  
  return(suppressWarnings(as.magpie(x)))
}
