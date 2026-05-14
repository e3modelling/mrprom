#' readOpenTEPES
#'
#' Read OPEN-TEPES annual electricity balance results
#' (oT_Result_BalanceEnergyPerArea_*.csv) per country and technology.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Christos Koumparakis
#'
#' @examples
#' \dontrun{
#' a <- readSource("OpenTEPES")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter %>% select mutate
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte

readOpenTEPES <- function() {
  files <- list.files(".", pattern = "^oT_Result_BalanceEnergyPerArea_.*\\.csv$")

  x <- NULL
  for (f in files) {
    x1 <- read.csv(f, check.names = FALSE, stringsAsFactors = FALSE)

    x1 <- pivot_longer(x1,
                       cols = -c("Period", "Scenario", "Technology"),
                       names_to = "region",
                       values_to = "value")

    x1 <- x1 %>%
      mutate(period = as.integer(.data[["Period"]]),
             scenario = .data[["Scenario"]],
             variable = .data[["Technology"]],
             unit = "GWh",
             model = "OpenTEPES") %>%
      select(c("model", "scenario", "region", "variable", "unit", "period", "value")) %>%
      filter(!is.na(.data[["value"]]))

    x1 <- as.quitte(x1) %>% as.magpie()
    x <- mbind(x, x1)
  }

  list(
    x = x,
    weight = NULL,
    description = c(
      category = "OPEN-TEPES results",
      type = "Electricity balance per area and technology",
      filename = "oT_Result_BalanceEnergyPerArea_*.csv",
      `Indicative size (MB)` = 1,
      dimensions = "4D",
      unit = "GWh",
      Confidential = "E3M"
    )
  )
}
