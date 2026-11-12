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
  x <- readSource("Industrycosts", convert = FALSE) %>%
    as.quitte() %>%
    select(-c("model", "scenario")) %>%
    mutate(
        value = value * 0.81/1000, #USD 2022/t to billion USD 2015 / million tonnes
        unit = "Billion USD2015 / Million tonnes" )  %>%
  select(-period) %>%
  crossing(period = 2010:2100) %>%
  as.quitte() %>%
  as.magpie()

  list(
    x = x,
    weight = NULL,
    unit = "various",
    description = "Industry costs (capex, opex, O&M) in USD15 per tonne of steel production"
  )
}
