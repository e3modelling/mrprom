#' @importFrom quitte as.quitte
#' @importFrom dplyr select %>%
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr full_join group_by mutate ungroup filter
#' @importFrom tidyr replace_na
#' @export
toolTShares <- function(historical, future) {
  print('ENTERED')
  print(historical)
  print(future)
  x <- historical %>%
    full_join(future, by = c("region", "variable", "period")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c(value.x, value.y)) %>%
    group_by(region, variable) %>%
    # arrange(period, .by_group = TRUE) %>%

    mutate(new_val = pmax(value - lag(value), 0))
    print(x)
    x <- x %>%
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
  print(x)
  return(x)
}
