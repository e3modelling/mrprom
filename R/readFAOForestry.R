#' Read FAOSTAT FAOForestry
#'
#' Reads the normalized FAOSTAT FAOForestry Sheets CSV file and converts
#' it into a magpie object.
#'
#' @return A list containing the data as a magpie object and its metadata.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' x <- readSource("FAOForestry")
#' }
#'
#' @importFrom dplyr select mutate filter distinct group_by across summarise %>%
#' @importFrom quitte as.quitte
#' @importFrom utils read.csv
#'
readFAOForestry <- function(subset = c("Area harvested", "Stocks")) {
  
  file <- "Forestry_E_All_Data_(Normalized).csv"
  
  if (!file.exists(file)) {
    stop("FAOSTAT file not found: ", file)
  }
  
  x <- utils::read.csv(
    file,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )  %>%  filter(
    Year %in% c(2010:2100)
  )
  
  x <- select(x, c(Area, Item, Element, Year, Unit, Value))
  
  x <- x %>%
    dplyr::select(
      region = Area,
      item = Item,
      variable = Element,
      period = Year,
      unit = Unit,
      value = Value
    ) %>%
    dplyr::mutate(
      period = as.integer(period),
      value = as.numeric(value)
    )
  
  x <- quitte::as.quitte(x)
  
  x <- as.magpie(x)
  
  return(
    list(x = x,
         weight = NULL,
         description = c(category = "FAOSTAT ProductionCrops",
                         type = "FAOSTAT ProductionCrops",
                         filename = "Production_Crops_Livestock_E_All_Data_(Normalized).csv",
                         `Indicative size (MB)` = 532,
                         dimensions = "2D",
                         unit = "various",
                         Confidential = "project")))
}