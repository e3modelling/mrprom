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
      x <- toolCountryFill(x, fill = NA)
    )
  )
  
  
  # # assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  qx <- as.quitte(x)
  qx_bu <- as.quitte(x)
  names(qx) <- sub("region", "CountryCode", names(qx))
  # ## add h12 mapping to dataset
  qx <- left_join(qx, h12, by = "CountryCode")
  # ## add new column containing regional mean value
  value <- NULL
  
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period"))
  qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  qx_bu <- select(qx_bu, -c("model", "scenario"))
  names(qx) <- sub("CountryCode", "region", names(qx))
  qx <- left_join(qx_bu, qx, by = c("region", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  x <- as.quitte(qx) %>% as.magpie()
  
  x <- x[as.character(getISOlist()), , ]
  x <- collapseDim(x,3)
  
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
