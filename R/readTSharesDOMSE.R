#' readTSharesDOMSE
#'
#' The dataset contains targets shares for DOMSE.
#'
#' @return The read-in targets shares data into a magpie object
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("TSharesDOMSE")
#' }
#'
#' @importFrom dplyr filter select group_by
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel
#' @importFrom quitte as.quitte
#'
readTSharesDOMSE <- function(subtype) {
  
  if (subtype == "SharesOP") {
    x <- read_excel("SharesOP.xlsx", col_names = TRUE)
    
    x <- select(x, -c("Sector", "Product", "Model"))
    
    x <- x %>% pivot_longer(!c("Region", "OP-Sector", "OP-Product"), names_to = "period", values_to = "value")
    
    x <- filter(x, !(`OP-Product` %in% c("OPEN-PROM Total", "Source Total")))
    
    x <- as.quitte(x) %>%
      interpolate_missing_periods(period = 2010:2100, expand.values = TRUE)
    
    x <- as.magpie(x)
  }
  
  if (subtype == "ProjectionsOP") {
    x <- read_excel("ProjectionsOP.xlsx", col_names = TRUE)
    
    x <- select(x, -c("Sector", "Product", "Model"))
    
    x <- x %>% pivot_longer(!c("Region", "OP-Sector", "OP-Product"), names_to = "period", values_to = "value")
    
    x <- filter(x, `OP-Product` %in% c("OPEN-PROM Total"))
    
    x <- x %>%
      mutate(value = ifelse(period > 2050 & value == 0, NA, value))
    
    x <- as.quitte(x) %>%
      interpolate_missing_periods(period = 2010:2100, expand.values = FALSE)
    
    # Takes value in 2050 vs 2040 (or last available decade)
    # Computes the true decade growth rate (CAGR)
    # Then compounds that growth forward to fill missing values
    
    # last_value / value_10y_ago → total growth factor over 10 years
    # (e.g., 2 means it doubled overall)
    # ^(1/10) → converts total growth into an average per year (geometric average)
    # - 1 → turns the growth factor into a percentage rate
    
    x <- x %>%
      mutate(period = as.numeric(as.character(period))) %>%
      group_by(`region`, `op-sector`, `op-product`) %>%
      arrange(period, .by_group = TRUE) %>%
      mutate(
        # Last period with actual data
        last_valid_period = max(period[!is.na(value)], na.rm = TRUE),
        
        # Value at last period and 10 years before
        last_value = value[period == last_valid_period],
        value_10y_ago = value[period == (last_valid_period - 10)],
        
        # CAGR (true decade growth rate)
        cagr = ifelse(
          !is.na(last_value) & !is.na(value_10y_ago),
          (last_value / value_10y_ago)^(1/10) - 1,
          NA_real_
        )
      ) %>%
      mutate(
        # Fill forward using CAGR
        value = {
          v <- value
          for (i in seq_along(v)) {
            if (is.na(v[i]) && period[i] > last_valid_period[i]) {
              v[i] <- v[i - 1] * (1 + cagr[i])
            }
          }
          v
        }
      ) %>%
      ungroup() %>%
      select(-last_valid_period, -last_value, -value_10y_ago, -cagr)
    
    x <- as.quitte(x)
    
    x <- as.magpie(x)
  }

  
  list(x = x,
       weight = NULL,
       description = c(category = "targets shares for DOMSE",
                       type = "targets shares for DOMSE",
                       filename = "SharesOP.xlsx",
                       `Indicative size (MB)` = 0.13,
                       dimensions = "3D",
                       unit = "shares",
                       Confidential = "E3M"))
  
}