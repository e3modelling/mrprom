#' Read FAOSTAT projections
#'
#' Reads the FAO projections CSV file and converts it into a magpie object.
#'
#' @return A list containing the data as a magpie object and its metadata.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' x <- readSource("FAOprojections")
#' }
#'
#' @importFrom dplyr select mutate filter distinct group_by across summarise %>%
#' @importFrom quitte as.quitte
#' @importFrom utils read.csv
#'
readFAOprojections <- function() {
  
  file <- "FOFA2050CountryData_Food-security.csv"
  
  if (!file.exists(file)) {
    stop("FAOSTAT file not found: ", file)
  }
  
  x <- utils::read.csv(
    file,
    check.names = FALSE,
    stringsAsFactors = FALSE
  ) 
  
  x <- select(x, c(Item, Element, CountryName, Scenario, Value, Units, Year))
  
  x <- x %>%
    dplyr::select(
      region = CountryName,
      item = Item,
      variable = Element,
      period = Year,
      unit = Units,
      value = Value,
      scenario = Scenario
    ) %>%
    dplyr::mutate(
      period = as.integer(period),
      value = as.numeric(value)
    )
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  return(
    list(x = x,
         weight = NULL,
         description = c(category = "FAOSTAT projections",
                         type = "FAOSTAT projections",
                         filename = "FOFA2050CountryData_Food-security.csv",
                         `Indicative size (MB)` = 20,
                         dimensions = "2D",
                         unit = "various",
                         Confidential = "project")))
}