#' calcIDataElecInd
#'
#' Use ENERDATA electricity production data to derive OPENPROM input parameter IDataElecInd
#'
#' @return  OPENPROM input data IDataElecInd
#'
#' @author Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataElecInd", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_rows
#'
calcIDataElecInd <- function() {
  transf <- calcOutput(
    type = "ITransfProcess",
    flow = "Out",
    aggregate = FALSE
  ) %>%
    as.quitte() %>%
    filter(sector == "CHP") %>%
    select(-sector) %>%
    mutate(value = ifelse(is.infinite(value), 1e-6, value))

  # If index is out of bounds [0.5, 2.5]; use global estimation
  world <- helperGetWorldElecInd(transf)

  data <- transf %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    mutate(value = ELC / STE) %>%
    select(-c("ELC", "STE")) %>%
    left_join(world, by = "period") %>%
    mutate(
      value = ifelse(value.x > 0.5 & value.x < 2.5 & !is.nan(value.x), value.x, value.y)
    ) %>%
    select(-c("value.x", "value.y")) %>%
    as.quitte() %>%
    as.magpie() %>%
    collapseDim(dim = 3.1)

  weights <- filter(transf, variable == "STE") %>%
    as.quitte() %>%
    as.magpie()
  weights[weights == 0] <- 1e-6

  list(
    x = data,
    weight = collapseDim(weights, dim = 3.1),
    unit = "ratio",
    description = "Elec / Steam ratio"
  )
}

# Helper --------------------------------------------------------
helperGetWorldElecInd <- function(data) {
  world <- group_by(data, period, variable) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    mutate(value = ELC / STE) %>%
    select(-c("ELC", "STE"))
  return(world)
}
