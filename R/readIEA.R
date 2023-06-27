#' readIEA
#'
#' Read in energy balances from International Energy Agency.
#'
#' @param subtype Type of data that should be read.
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA", subtype = "MAINELEC")
#' }
#'
#' @importFrom utils read.csv2
#'
readIEA <- function(subtype = "MAINELEC") {
  if (subtype != "MAINELEC") {
    x <- read.csv2("ieaWB.csv")
    names(x) <- c("region", "period", "product", "flow", "value")
    x <- x[-1, ]
    x[["region"]] <- factor(x[["region"]])
    x[["product"]] <- factor(x[["product"]])
    x[["flow"]] <- factor(x[["flow"]])
    x[["period"]] <- as.numeric(x[["period"]])
    x[["value"]] <- as.numeric(x[["value"]])
    saveRDS(object = x, file = "iea.rds")
  }


  x <- readRDS("iea.rds")
  levels(x[["region"]])[1] <- "Curacao"
  levels(x[["region"]])[2] <- "Cote d'Ivoire"
  levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]), mapping =
                                                 c("Bolivarian Republic of Venezuela" = "VEN",
                                                   "China (P.R. of China and Hong Kong, China)" = "CHA",
                                                   "Kingdom of Eswatini" = "SWZ",
                                                   "Republic of the Congo" = "COG",
                                                   "Republic of Turkiye" = "TUR",
                                                   "World" = "GLO"))
  x <- filter(x, !is.na(x[["region"]]))
  x <- filter(x, x[["flow"]] == subtype)
  return(as.magpie(x))
}
