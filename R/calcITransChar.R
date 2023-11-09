#' calcITransChar
#'
#' Use IRF data to derive OPENPROM input parameter iTransChar
#'
#' @return  OPENPROM input data iTransChar
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "ITransChar", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte interpolate_missing_periods

calcITransChar <- function() {

  a <- readSource("IRF", subtype = "total-van,-pickup,-lorry-and-road-tractor-traffic")
  #million motor vehicle km/yr
  KM_VEH_TRUCK <- a * 1000
  #Thousands km/yr
  a3 <- readSource("IRF", subtype = "total-vans,-pickups,-lorries-and-road-tractors-in-use")
  a3 <- a3[, Reduce(intersect, list(getYears(a3), getYears(KM_VEH_TRUCK))), ]
  KM_VEH_TRUCK <- KM_VEH_TRUCK[, Reduce(intersect, list(getYears(a3), getYears(KM_VEH_TRUCK))), ]
  KM_VEH_TRUCK <- KM_VEH_TRUCK / a3
  #Thousands km/veh
  a2 <- readSource("IRF", subtype = "passenger-car-traffic")
  #million motor vehicle km/yr
  KM_VEH <- a2 * 1000
  #Thousands km/yr
  a4 <- readSource("IRF", subtype = "passenger-cars-in-use")
  a4 <- a4[, Reduce(intersect, list(getYears(a4), getYears(KM_VEH))), ]
  KM_VEH <- KM_VEH[, Reduce(intersect, list(getYears(a4), getYears(KM_VEH))), ]
  KM_VEH <- KM_VEH / a4
  #Thousands km/veh

  getNames(KM_VEH) <- "KM_VEH"
  getSets(KM_VEH) <- c("region", "period", "variable")
  getNames(KM_VEH_TRUCK) <- "KM_VEH_TRUCK"
  getSets(KM_VEH_TRUCK) <- c("region", "period", "variable")
  q1 <- as.quitte(KM_VEH)
  q2 <- as.quitte(KM_VEH_TRUCK)

  q3 <- matrix(0, nrow(q1), length(q1))
  q3 <- as.data.frame(q3)
  q3[, 1:6] <- q1[, 1:6]
  q3[, 7] <- NA
  q3[, 4] <- "OCCUP_CAR"

  names(q3) <- names(q1)

  x <- rbind(q1, q3, q2)


  # complete incomplete time series
  z <- mbind(a, a2)
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = getYears(z, as.integer = TRUE), expand.values = TRUE)
  qx_bu <- qx
  # assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv")
  names(qx) <- sub("region", "CountryCode", names(qx))
  ## add h12 mapping to dataset
  qx <- left_join(qx, h12, by = "CountryCode")
  ## add new column containing regional mean value
  value <- NULL
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "variable"))
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
       unit = "Thousands km/yr",
       description = "IRF;")

}
