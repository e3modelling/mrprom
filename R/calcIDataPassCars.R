#' calcIDataPassCars
#'
#' Use data to derive OPENPROM input parameter IDataPassCars
#'
#' @return  OPENPROM input data iDataPassCars
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataPassCars", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte

calcIDataPassCars <- function() {

  y <- readSource("Eurostat_ELVS", convert = TRUE)

  a <- readSource("IRF", subtype = "total-vehicles-in-use")

  a <- a[, Reduce(intersect, list(getYears(a), getYears(y))), ]
  y <- y[, Reduce(intersect, list(getYears(a), getYears(y))), ]

  x <- y / a

  getNames(x) <- "PC"
  getSets(x) <- c("region", "period", "unit")

  k <- readSource("BoT")

  getNames(y) <- "PC"
  getSets(y) <- c("region", "period", "unit")

  k <- as.quitte(k) %>%
    interpolate_missing_periods(period = getYears(a, as.integer = TRUE), expand.values = TRUE)

  k <- as.quitte(k) %>% as.magpie()

  a <- a[, Reduce(intersect, list(getYears(a), getYears(k))), ]
  k <- k[, Reduce(intersect, list(getYears(a), getYears(k))), ]
  a <- a["USA", , ]
  p <- k / a

  x["USA", , ] <- p

  qx <- as.quitte(x)
  qx_bu <- qx
  # assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv")
  names(qx) <- sub("region", "CountryCode", names(qx))
  ## add h12 mapping to dataset
  qx <- left_join(qx, h12, by = "CountryCode")
  ## add new column containing regional mean value
  value <- NULL
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "unit", "variable"))
  names(qx) <- sub("CountryCode", "region", names(qx))
  qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  qx_bu <- select(qx_bu, -c("model", "scenario"))
  ## assign to countries with NA, their H12 region mean
  value.x <- NULL
  value.y <- NULL
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  ## assign to countries that still have NA, the global mean
  qx_bu <- qx
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "variable", "unit"))
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))

  x <- as.quitte(qx) %>% as.magpie()

  list(x = x,
       weight = NULL,
       unit = "reuse_pc",
       description = "reuse_pc")
}
