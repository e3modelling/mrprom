#' calcISuppRatePrimProd
#'
#' Use IEA data to derive OPENPROM input parameter iSuppRatePrimProd.
#' It is the ratio of primary production over the total energy supply
#' for each country.
#'
#' @return  OPENPROM input data iSuppRatePrimProd.
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "ISuppRatePrimProd", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
calcISuppRatePrimProd <- function() {
  # Gross inland consumption
  tes <- calcOutput(
    type = "ITotEneSupply", subtype = "TES", aggregate = FALSE
  ) %>%
    as.quitte()

  primary <- calcOutput(
    type = "ITotEneSupply", subtype = "Primary", aggregate = FALSE
  ) %>%
    as.quitte() %>%
    select(region, period, variable, value)

  # Calculate the ratio of primary / TES for each country, year, and variable
  ratio <- primary %>%
    left_join(tes, by = c("region", "period", "variable")) %>%
    mutate(
      value = value.x / value.y,
      value = ifelse(is.na(value) | is.infinite(value), 0, value)
    ) %>%
    select(-c("value.x", "value.y")) %>%
    as.quitte() %>%
    as.magpie() %>%
    # FIXME: Proper impute must be done. For now fill with zero.
    toolCountryFill(fill = 0)

  ratio[is.na(ratio)] <- 0

  weights <- tes %>%
    mutate(value = ifelse(value == 0, 1e-6, value)) %>%
    as.magpie()

  list(
    x = ratio,
    weight = weights,
    unit = "Rate",
    description = "IEA; Primary Energy / TES"
  )
}
