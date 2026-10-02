#' Read FAOSTAT Food Balance Sheet data
#'
#' Reads the normalized FAOSTAT Food Balance Sheets CSV file and converts
#' it into a magpie object.
#'
#' @return A list containing the data as a magpie object and its metadata.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' x <- readSource("FAO")
#' }
#'
#' @importFrom dplyr select mutate filter distinct group_by across summarise %>%
#' @importFrom quitte as.quitte
#' @importFrom utils read.csv
#'
readFAO <- function(subset = "Food supply (kcal/capita/day)") {
  
  file <- "FoodBalanceSheets_E_All_Data_(Normalized).csv"
  
  if (!file.exists(file)) {
    stop("FAOSTAT file not found: ", file)
  }
  
  x <- utils::read.csv(
    file,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )  %>%  filter(
    Element %in% subset
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
      model = "FAOSTAT",
      scenario = "historical",
      period = as.integer(period),
      value = as.numeric(value)
    )
  
  x <- quitte::as.quitte(x)
  
  x <- x %>%
    dplyr::distinct()
  
  x <- x %>%
    dplyr::group_by(
      dplyr::across(-value)
    ) %>%
    dplyr::summarise(
      value = max(value, na.rm = TRUE),
      .groups = "drop"
    )
  
  x <- as.quitte(x)
  x <- as.magpie(x)

  return(
    list(x = x,
         weight = NULL,
         description = c(category = "FAOSTAT Food Balance Sheets",
                         type = "FAOSTAT Food Balance Sheets",
                         filename = "FoodBalanceSheets_E_All_Data_(Normalized).csv",
                         `Indicative size (MB)` = 618,
                         dimensions = "2D",
                         unit = "various",
                         Confidential = "project")))
}