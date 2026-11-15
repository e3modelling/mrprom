#' Read the NDC and LTT source workbook
#'
#' Read the policy targets and the historical emissions used to weight target coverage.
#'
#' @return A madrat source list containing the `targets` and `historicalEmissions` tables.
#' @author Anastasis Giannousakis, Alexandros Tsimpoukis, Songmin Yu
#' @importFrom readxl read_excel
#' @examples
#' \dontrun{
#' readSource("NDC_LTT_NECP", convert = FALSE)
#' }
readNDC_LTT_NECP <- function() {
  file <- "NDC_LTT_NECP_targets_05122025.xlsx"
  x <- list(
    targets = read_excel(file, sheet = "global_targets", .name_repair = "minimal"),
    historicalEmissions = read_excel(file, sheet = "GHG_2019_histo", .name_repair = "minimal")
  )
  list(
    x = x,
    weight = NULL,
    unit = "Mixed policy metadata and Mt CO2e/yr",
    class = "list",
    description = "Global targets from NDCs and LTTs with historical emissions"
  )
}
