#' Emissions-weighted coverage of annual NDC and long-term caps
#'
#' Coverage uses the share of known 2019 greenhouse gas emissions represented
#' by countries with an accepted cap. A reported zero cap counts as present.
#' Countries without a historical weight contribute zero to both sums.
#'
#' @return A madrat list with target availability, aggregation weights and metadata.
#' @author Songmin Yu
#' @examples
#' \dontrun{
#' calcOutput("NDC_LTT_NECP_Coverage", aggregate = TRUE)
#' }
calcNDC_LTT_NECP_Coverage <- function() {
  x <- toolNdcLttArrays()
  list(
    x = x$present,
    weight = x$weights,
    unit = "fraction",
    description = "Share of known 2019 regional GHG emissions with an accepted target"
  )
}
