#' Read FAOSTAT FAOLandUse
#'
#' Reads the FAO projections CSV file and converts it into a magpie object.
#'
#' @return A list containing the data as a magpie object and its metadata.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' x <- readSource("FAOLandUse")
#' }
#'
#' @importFrom dplyr select mutate filter distinct group_by across summarise %>%
#' @importFrom quitte as.quitte
#' @importFrom utils read.csv
#'
readFAOLandUse <- function() {
  
  file <- "Inputs_LandUse_E_All_Data_(Normalized).csv"
  
  if (!file.exists(file)) {
    stop("FAOSTAT file not found: ", file)
  }
  
  x <- utils::read.csv(
    file,
    check.names = FALSE,
    stringsAsFactors = FALSE
  ) %>%  filter(
    Year %in% c(2010:2100)
  ) 
  
  x <- select(x, c(Item, Element, Area, Value, Unit, Year))
  
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
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  return(
    list(x = x,
         weight = NULL,
         description = c(category = "FAOSTAT LandUse",
                         type = "FAOSTAT LandUse",
                         filename = "Inputs_LandUse_E_All_Data_(Normalized).csv",
                         `Indicative size (MB)` = 48,
                         dimensions = "2D",
                         unit = "various",
                         Confidential = "project")))
}