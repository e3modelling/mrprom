#' calciProductionIS
#' 
#' 
#'
#' @return magpie object with OPENPROM input data iProductionIS
#'
#' @author Margarita Efthymiadou
#' 
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "iProductionIS", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% mutate
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom madrat toolGetMapping
#' @importFrom tidyr crossing
calciProductionIS <- function() {

  # --- read data ---
  x <-  readSource("WSA_Prod_DRI") %>%
    as.quitte() %>%
    select(-c("model", "scenario", "unit")) %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    mutate(
      #value = ifelse(is.na(value), 0, value), # !ERROR: NEEDS double check
      BOF = Total_Production * Oxygen / 100,
      EAF = Total_Production * Electric / 100,
      other = Total_Production * (Open_hearth + Other) / 100,
      DRI = DRI_Production * ifelse(region %in% c("IRN", "LBY"), 0.7, 0.9),
      SCREAF = EAF - DRI
    ) %>%
    select(region, period, BOF, DRI, SCREAF, other) %>%
    pivot_longer(cols = c("BOF", "DRI", "SCREAF", "other"), names_to = "variable", values_to = "value") %>%
    mutate(value = pmax(replace_na(value, 0), 0)) %>%
    as.quitte() %>%
    as.magpie()

  
  
  list(
    x = x,
    weight = NULL,
    unit = "Mt",
    description = "Historical production BFBOF, DREAF and SCREAF in IS route"
  )
}
