#' calcTShareTechPG
#'
#' Compute total-mix shares of electricity production per OPEN-PROM technology
#' for the calibration of the OPEN-PROM variable V04ShareTechPG.
#'
#' Pulls the production levels from \code{calcOutput("TProdElec",
#' subtype = subtype)} and divides each tech with the total production.
#' #' With \code{subtype = "OpenTEPES"} TProdElec already includes the OPEN-TEPES
#' NT2030 anchor for the 27 EU countries. With ' \code{subtype = "default"} the shares come from the pure PRIMES + IEA production trajectory.
#'
#' @param subtype Forwarded to \code{calcTProdElec}. One of:
#'   \itemize{
#'     \item \code{"default"}   - shares of pure PRIMES + IEA production.
#'     \item \code{"OpenTEPES"} - shares of OPEN-TEPES-anchored production.
#'   }
#'
#' @return magpie object
#'
#' @author Christos Koumparakis
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "TShareTechPG", aggregate = FALSE)
#' a <- calcOutput(type = "TShareTechPG", subtype = "OpenTEPES",
#'                 aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>% mutate select group_by ungroup rename
calcTShareTechPG <- function(subtype = "default") {

  prod <- calcOutput("TProdElec", subtype = subtype, aggregate = FALSE) %>%
    as.quitte() %>%
    select(c("region", "variable", "period", "value")) %>%
    mutate(region = as.character(region),
           variable = as.character(variable))

  shares_df <- prod %>%
    group_by(region, period) %>%
    mutate(total = sum(value, na.rm = TRUE),
           share = ifelse(total > 0, value / total, 0)) %>%
    ungroup()

  x_df <- shares_df %>%
    select(c("region", "variable", "period", "share")) %>%
    rename(value = "share")

  w_df <- shares_df %>%
    select(c("region", "variable", "period", "total")) %>%
    rename(value = "total") %>%
    mutate(value = ifelse(value <= 0, 1e-6, value))

  x <- as.quitte(x_df) %>% as.magpie()
  w <- as.quitte(w_df) %>% as.magpie()

  list(
    x = x,
    weight = w,
    unit = "fraction",
    description = "Total share of electricity production per OPEN-PROM technology"
  )
}
