#' readEurostatHP
#'
#' Read EurostatHP
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("EurostatHP")
#' }
#'
#' @importFrom dplyr select mutate across
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel
#' @importFrom quitte as.quitte
#' @importFrom readxl read_excel
#'
readEurostatHP <- function() {
  
  x <- read_excel("heatpump_mix.xlsx", col_names = TRUE, skip = 1)
  
  x <- x %>%
    select(hp_tech, unit, `geo\\TIME_PERIOD`, matches("^\\d{4}$")) %>%
    mutate(across(
      matches("^\\d{4}$"),
      ~ as.numeric(na_if(as.character(.x), ":"))
    )) %>%
    pivot_longer(
      cols = matches("^\\d{4}$"),
      names_to = "period",
      values_to = "value"
    ) %>%
    mutate(period = as.integer(period))
  
  names(x)[names(x) == "geo\\TIME_PERIOD"] <- "region"
  names(x)[names(x) == "hp_tech"] <- "variable"
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "EurostatHP",
                       type = "EurostatHP",
                       filename = "heatpump_mix.xlsx",
                       `Indicative size (MB)` = 0.26,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "E3M"))
}
