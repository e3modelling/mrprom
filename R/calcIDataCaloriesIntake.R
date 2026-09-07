#' calcIDataCaloriesIntake
#'
#' @return  Magpie object with the Food supply (kcal/capita/day)
#'
#' @author Fotis Sioutas
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataCaloriesIntake", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% mutate pull
#' @importFrom tidyr replace_na
#' @importFrom quitte as.quitte

calcIDataCaloriesIntake <- function() {

  x <- readSource("FAO", convert = TRUE)
  x <- collapseDim(x, 3.4)
  x <- collapseDim(x, 3.3)
  x <- collapseDim(x, 3.2)
  x <- collapseDim(x, 3.1)
  x[is.na(x)] <- 0
  
  FAO_item <- toolGetMapping(
    name = "FAO_item_mapping_no_double_count_v3.csv",
    type = "sectoral",
    where = "mrprom") %>%
    filter(category != "DROP")
  
  xAggr <- toolAggregate(x[,,FAO_item[["item"]]], dim=3, rel = FAO_item, from="item", to="category")
  
  
  
  
  
  list(
    x = data,
    weight = NULL,
    unit = "kcal/capita/day",
    description = "Food supply (kcal/capita/day)"
  )
}
