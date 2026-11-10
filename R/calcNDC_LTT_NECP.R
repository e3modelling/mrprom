#' Annual NDC and long-term emissions caps
#'
#' Returns country-level caps in Mt CO2e per year. The dedicated NDC rows use
#' targets for the specified year, preferring revised pledges. The dedicated
#' long-term rows carry the latest eligible cap forward from its deadline.
#' A missing conditional cap falls back to the unconditional cap from the
#' same pledge. The combined rows retain the NDC/LTT series used by existing callers.
#'
#' Missing caps are stored as zero. Use the companion
#' `NDC_LTT_NECP_Coverage` output to determine availability.
#'
#' @return A madrat list with a country-by-year magclass object and metadata.
#' @author Anastasis Giannousakis, Alexandros Tsimpoukis, Songmin Yu
#' @examples
#' \dontrun{
#' calcOutput("NDC_LTT_NECP", aggregate = FALSE)
#' }
calcNDC_LTT_NECP <- function() {
  x <- toolNdcLttArrays()
  list(
    x = x$values,
    weight = NULL,
    unit = "Mt CO2e/yr",
    description = "Annual NDC and long-term emissions caps"
  )
}
