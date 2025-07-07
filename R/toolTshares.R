#' @importFrom quitte as.quitte
#' @importFrom dplyr select %>%
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr full_join group_by mutate ungroup filter
#' @importFrom tidyr replace_na
#' @export
toolTShares <- function(future) {
  x <- future %>%
    group_by(region, variable) %>%
    mutate(new_val = pmax(value - lag(value), 0)) %>%
    group_by(region, period) %>%
    mutate(
      value = new_val / sum(new_val, na.rm = TRUE),
      value = ifelse(is.nan(value), 0, value)
    ) %>%
    select(-new_val) %>%
    filter(period >= 2021) %>%
    mutate(
      value = replace_na(value, 0),
      value = ifelse(is.na(value), 0, value)
    ) %>%
    ungroup()
  return(x)
}
