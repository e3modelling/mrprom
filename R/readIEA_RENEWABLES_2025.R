#' readIEA_RENEWABLES_2025
#'
#' @param subtype fuel : Type of data that should be read.
#' Available types are:
#' \itemize{
#' \item `ELECTRICITY`
#' \item `TRANSPORT`
#' \item `HEAT`
#' }
#' 
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_RENEWABLES_2025", subtype = "ELECTRICITY", convert = TRUE)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr select filter
#' @importFrom quitte as.quitte
#'
readIEA_RENEWABLES_2025 <- function(subtype = "ELECTRICITY") {
  
  if (subtype == "ELECTRICITY") {
    x <- read_excel("RENEWABLES2025_Databook.xlsx", sheet = "ELECTRICITY")
    x <- select(x, c("Code Region", "Year" , "Code Product", "Code Flow", "Code Case", "Value", "Flow"))
    x["type"] <- "ELECTRICITY"
    names(x) <- sub("Code Region", "region", (names(x)))
    names(x) <- sub("Year", "period", (names(x)))
    names(x) <- sub("Code Product", "product", (names(x)))
    names(x) <- sub("Code Flow", "flow", (names(x)))
    names(x) <- sub("Code Case", "case", (names(x)))
    names(x) <- sub("Flow", "unit", (names(x)))
    x[,"unit"] <- sub(".*\\(([^)]+)\\).*", "\\1", x[,"unit"])
    
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "TRANSPORT") {
    x <- read_excel("RENEWABLES2025_Databook.xlsx", sheet = "TRANSPORT")
    x <- select(x, c("Code Region", "Year" , "Code Product", "Code Flow", "Case", "Value", "Unit"))
    x["type"] <- "TRANSPORT"
    names(x) <- sub("Code Region", "region", (names(x)))
    names(x) <- sub("Year", "period", (names(x)))
    names(x) <- sub("Code Product", "product", (names(x)))
    names(x) <- sub("Code Flow", "flow", (names(x)))
    names(x) <- sub("Code Case", "case", (names(x)))
    
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "HEAT") {
    x <- read_excel("RENEWABLES2025_Databook.xlsx", sheet = "HEAT")
    x <- select(x, c("REGION_CODE", "YEAR" , "PRODUCT_CODE", "VALUE", "UNIT", "SECTOR"))
    x["type"] <- "HEAT"
    names(x) <- sub("REGION_CODE", "region", (names(x)))
    names(x) <- sub("YEAR", "period", (names(x)))
    names(x) <- sub("SECTOR", "sector", (names(x)))
    
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  
  
  list(x = x,
       weight = NULL,
       description = c(category = "RENEWABLES2025_Databook",
                       type = "RENEWABLES2025_Databook",
                       filename = "RENEWABLES2025_Databook.xlsx",
                       `Indicative size (MB)` = 0.12,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "E3M"))
}
