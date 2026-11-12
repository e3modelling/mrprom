#' calcIndustryCosts
#'
#'
#' @return magpie object with OPENPROM input data calcIndustryCosts (capex, opex, O&M)
#'
#' @author Margarita Efthymiadou
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IndustryCosts", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% mutate
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom madrat toolGetMapping
#' @importFrom tidyr crossing
calcIndustryCosts <- function() {
  # --- read data ---
  data <- readSource("Industrycosts", convert = FALSE) %>%
    as.quitte() %>%
    select(-c("model", "scenario")) %>%
    mutate(
        value = value * 0.81, #EUR 2020 to USD 2015
        unit = "Billion USD2015 / Million tonnes" )

  
    # --- interpolate each technology separately ---
  years <- 2010:2100

  data <- data %>%
    group_by(technology, variable, unit) %>%
    group_modify(~ {

      original <- .x

      tibble(
        period = years,
        value = approx(
          x = original$period,
          y = original$value,
          xout = years,
          method = "linear",
          rule = 2
        )$y
      )

    }) %>%
    ungroup()
  
  x <- data %>%
    select(period, technology, variable, unit, value) %>%
    as.quitte() %>%
    as.magpie()

  list(
    x = x,
    weight = NULL,
    unit = "various",
    description = "Industry costs (capex, opex, O&M) in USD15 per tonne of steel production"
  )
}
