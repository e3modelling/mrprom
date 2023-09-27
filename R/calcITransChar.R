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
#' @importFrom dplyr %>% select
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte

calcITransChar <- function() {
  
  a <- readSource("IRF", subtype = "total-van,-pickup,-lorry-and-road-tractor-traffic")
  #million motor vehicle km/yr
  KM_VEH_TRUCK <- a*1000
  #Thousands km/yr
  a2 <- readSource("IRF", subtype = "passenger-car-traffic")
  #motor vehicle km/yr
  KM_VEH <- a2/1000
  #Thousands km/yr
  
  getNames(a) <- "KM_VEH"
  getSets(a) <- c("region", "period", "dummy")
  getNames(a2) <- "KM_VEH_TRUCK"
  getSets(a2) <- c("region", "period", "dummy")
  q1 <- as.quitte(a)
  q2 <- as.quitte(a2)

  q3 <- matrix(0, nrow(q1), length(q1))
  q3 <- as.data.frame(q3)
  q3[, 1:6] <- q1[, 1:6]
  q3[, 7] <- NA
  q3[, 8] <- "OCCUP_CAR"
  
  names(q3) <- names(q1)
  
  x <- rbind(q1,q3,q2)

  x["value"][x["value"] == 0] <- NA
  # complete incomplete time series
  z <- mbind(a,a2)
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = getYears(z, as.integer = TRUE), expand.values = TRUE)  
  qx_bu <- qx
  # assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv")
  names(qx) <- sub("region", "CountryCode", names(qx))
  ## add h12 mapping to dataset
  qx <- left_join(qx, h12, by="CountryCode")
  ## add new column containing regional mean value
  value <- NULL
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "dummy", "variable"))
  names(qx) <- sub("CountryCode", "region", names(qx))
  qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  qx_bu <- select(qx_bu, -c("model", "scenario"))
  ## assign to countries with NA, their H12 region mean
  value.x <- NULL
  value.y <- NULL
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "dummy", "unit")) %>% 
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>% 
    select(-c("value.x", "value.y"))
  ## assign to countries that still have NA, the global mean
  qx_bu <- qx
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "dummy", "variable", "unit"))
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "dummy", "unit")) %>% 
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>% 
    select(-c("value.x", "value.y"))
  
  x <- select(qx, "region", "value", "period", "dummy")
  x <- pivot_wider(x, names_from = "period",values_from = "value") 
  
  fheader <- paste("dummy,dummy", paste(colnames(x)[3 : length(colnames(x))], collapse = ","), sep = ",")
  writeLines(fheader, con = paste0("iTransChar", ".csv"))
  x <- as.matrix(x)
  write.table(x,
              quote = FALSE,
              row.names = FALSE,
              file = paste0("iTransChar", ".csv"),
              sep = ",",
              col.names = FALSE,
              append = TRUE)
  
  x <- as.quitte(qx) %>% as.magpie()
  
  list(x = x,
       weight = NULL,
       unit = "Thousands km/yr",
       description = "IRF;")
  
 
}