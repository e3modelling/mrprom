#' readIMF
#'
#' Read IMF GDP
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IMF", convert = TRUE)
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readIMF <- function() {
  
  x <- read.csv("IMF_WEO_2025_6.0.0.csv", header = FALSE)
  x[1,56:length(x)] <- as.numeric(x[1,56:length(x)])
  # Assign first row as column names
  colnames(x) <- x[1, ]
  
  # Remove the first row (now redundant)
  x <- x[-1, ]
  
  x <- filter(x, INDICATOR %in% c("Gross domestic product (GDP), Constant prices, Domestic currency",
                                  "Gross domestic product (GDP), Constant prices, Per capita, Domestic currency",
                                  "Gross domestic product (GDP), Constant prices, Percent change",
                                  "Gross domestic product (GDP), Current prices, Per capita, US dollar",
                                  "Gross domestic product (GDP), Price deflator, Index"))
  
  
  x <- pivot_longer(x, where(is.numeric), names_to = "period", values_to = "value")
  x <- x[,c("SERIES_CODE","INDICATOR","period","value","UNIT")]
  names(x) <- sub("SERIES_CODE","region",names(x))
  names(x) <- sub("INDICATOR","variable",names(x))
  
  x[["region"]] <- sub("\\..*", "", x[["region"]])
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "GDP",
                       type = "GDP",
                       filename = "IMF_WEO_2025_6.0.0.csv",
                       `Indicative size (MB)` = 30,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "E3M"))
}
