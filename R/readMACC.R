#' readMACC
#'
#' Read the CH4/N2O and F-gas MAC curve XLSX files and convert them into
#' structured data frames. The function extracts only sheets that contain
#' SSP2-compatible information (i.e., sheets whose names start with "SSP2"),
#' and returns them as a list of data frames.
#'
#' The CH4/N2O dataset provides SSP2 baseline emissions (in Mt C-equivalent)
#' and marginal abatement cost (MAC) curves expressed as relative emission
#' reduction potentials. The F-gas dataset provides SSP2 baseline emissions
#' in kilotonnes of gas mass and MAC curves expressed as reductions relative
#' to baseline, along with costs in 2005 USD per tonne of C-equivalent.
#'
#' The resulting data structure can be used directly in IAMs that apply
#' emission-based MAC curves rather than emission-factor-based approaches
#'
#' @return A named list containing SSP2 CH4/N2O and SSP2 F-gas MAC curve
#'         data frames.
#'
#' @author Anastasis Giannousakis, Alexandros Tsimpoukis
#'
#' @examples
#' \dontrun{
#' a <- readSource("MACC")
#' }
#' 
#' @importFrom readxl read_excel
#'

readMACC <- function() {

  # Paths for the files
  pthCh4N2o <- "Data_MAC_CH4N2O_Harmsen-et-al_PBL_2019.xlsx"
  pthFgases <- "Data_MAC_F-gases_Harmsen-et-al_PBL_2019.xlsx"

  dfCh4N2o <- readMACCSheets(pthCh4N2o, filterSsp2 = TRUE)
  dfFgases <- readMACCSheets(pthFgases, filterSsp2 = FALSE)

  x <- list(dfCh4N2o, dfFgases)

  list(
    x = x,
    weight = NULL,
    unit = "Mt C-eq for CH4/N2O baselines; kt gas for F-gases",
    class = "list",
    description = "CH4/N2O and F-gas MAC curve datasets (SSP2-compatible)"
  )
}

# Helper ------------------------------------------------------------------------------------
readMACCSheets <- function(filePath, filterSsp2 = FALSE) {

  sheets <- excel_sheets(filePath)

  # Only filter CH4/N2O sheets
  if (filterSsp2) {
    sheets <- sheets[grepl("^SSP2", sheets)]  # only sheets STARTING with SSP2
  }

  data <- lapply(sheets, function(sheetName) {
    read_excel(filePath, sheet = sheetName)
  })

  names(data) <- sheets
  return(data)
}