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
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#'
readIEA <- function(subtype = "MAINELEC") {
  if (!file.exists("iea.rds")) {
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
  
  if (subtype != "all") {
    x <- filter(x, x[["flow"]] == subtype)
  }
  x["unit"] <- "various"
  levels(x[["region"]])[1] <- "Curacao"
  levels(x[["region"]])[2] <- "Cote d'Ivoire"
  x <- as.quitte(x)
  x <- as.magpie(x) 
  
  list(x = x,
       weight = NULL,
       description = c(category = "Energy balances",
                       type = "Energy balances until period 2021",
                       filename = "ieaWB.csv",
                       `Indicative size (MB)` = 3000,
                       dimensions = "3D",
                       unit = "varius",
                       Confidential = "E3M"))
}
