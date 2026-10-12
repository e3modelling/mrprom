#' readWorldBankPriceLevelIndex
#'
#' @return magpie object with the requested output data about
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("WorldBankPriceLevelIndex")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom stringr str_remove
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% across mutate
#' @importFrom readxl read_excel
#' @importFrom stringi stri_escape_unicode
#'

readWorldBankPriceLevelIndex <- function() {
  
  x <- read.csv("API_PA.NUS.GDP.PLI_DS2_en_csv_v2_10299.csv",
                skip = 4,
                check.names = FALSE)
  
  x <- x %>%
    select(region = `Country Code`, matches("^[0-9]{4}$")) %>%
    pivot_longer(
      cols = -region,
      names_to = "period",
      values_to = "value"
    ) %>%
    filter(!is.na(value)) %>%
    mutate(period = as.integer(period)) %>%
    group_by(region) %>%
    slice_max(order_by = period, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  x <- select(x, - period)
  
  x <- as.quitte(x) %>% as.magpie()
  
  x <- x / 100
  
  suppressMessages(
    suppressWarnings(
      x <- toolCountryFill(x, fill = 1)
    )
  )
  
  x <- x[as.character(getISOlist()), , ]
  
  
  list(x = x,
       weight = NULL,
       description = c(category = "WorldBankPriceLevelIndex",
                       type = "WorldBankPriceLevelIndex",
                       filename = "API_PA.NUS.GDP.PLI_DS2_en_csv_v2_10299.csv",
                       `Indicative size (MB)` = 0.178,
                       dimensions = "2D",
                       unit = "PriceLevelIndex",
                       Confidential = "open"))
  
}
