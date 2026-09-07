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
#' @importFrom dplyr select mutate
#' @importFrom quitte as.quitte
#' @importFrom utils read.csv
#'
readFAO <- function() {
  
  file <- "FoodBalanceSheets_E_All_Data_(Normalized).csv"
  
  if (!file.exists(file)) {
    stop("FAOSTAT file not found: ", file)
  }
  
  x <- utils::read.csv(
    file,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  x <- x %>%
    dplyr::select(
      area_code = `Area Code`,
      m49 = `Area Code (M49)`,
      region = Area,
      item_code = `Item Code`,
      item_code_fbs = `Item Code (FBS)`,
      item = Item,
      element_code = `Element Code`,
      element = Element,
      period = Year,
      unit = Unit,
      value = Value,
      flag = Flag,
      note = Note
    ) %>%
    dplyr::mutate(
      model = "FAOSTAT",
      scenario = "historical",
      period = as.integer(period),
      value = as.numeric(value)
    )
  
  x <- quitte::as.quitte(x)
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