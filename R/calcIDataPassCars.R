#' calcIDataPassCars
#'
#' Use data to derive OPENPROM input parameter iDataPassCars
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
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIDataPassCars <- function() {
  
  x <- readSource("Eurostat_ELVS")
  
  x <- toolCountryFill(x, fill = NA) #nolint
  
  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = getYears(x, as.integer = TRUE), expand.values = TRUE)  
  qx_bu <- qx
  # assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv")
  names(qx) <- sub("region", "CountryCode", names(qx))
  ## add h12 mapping to dataset
  qx <- left_join(qx, h12, by="CountryCode")
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
  
  z <- qx
  
  x <- select(qx, "region", "unit", "period", "value")
  x <- pivot_wider(x, names_from = "period",values_from = "value") 
  
  fheader <- paste("dummy,dummy", paste(colnames(x)[3 : length(colnames(x))], collapse = ","), sep = ",")
  writeLines(fheader, con = paste0("iDataPassCars", ".csv"))
  x <- as.matrix(x)
  write.table(x,
              quote = FALSE,
              row.names = FALSE,
              file = paste0("iDataPassCars", ".csv"),
              sep = ",",
              col.names = FALSE,
              append = TRUE)
  
  x <- as.quitte(z) %>% as.magpie()
  
  list(x = x,
       weight = NULL,
       unit = "percentage_of_reuse_pc",
       description = "percentage_of_reuse_pc")
}