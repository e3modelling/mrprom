#' readNDC_LTT_NECP
#'
#' Read the Global targets from NDCs and LTTs from XLSX file and convert them into
#' structured data frames.
#'
#' @return A named list containing targets dataframes
#'
#' @author Anastasis Giannousakis, Alexandros Tsimpoukis
#'
#' @examples
#' \dontrun{
#' a <- readSource("NDC_LTT_NECP")
#' }
#' 
#' @importFrom readxl read_excel
#'

readNDC_LTT_NECP <- function() {

  fileNDCs <- "NDC_LTT_NECP_targets_05122025.xlsx"
  sheetName <- "global_targets"
  dataNDCs <- read_excel(fileNDCs, sheet = sheetName)
  x <- list(dataNDCs)
  list(
    x = x,
    weight = NULL,
    unit = "(%) and Mt C-eq",
    class = "list",
    description = "Global targets from NDCs and LTTs"
  )
}