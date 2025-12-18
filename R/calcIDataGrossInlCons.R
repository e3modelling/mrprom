#' calcIDataGrossInlCons
#'
#' Use data from IEA to derive OPENPROM input parameter iDataGrossInlCons
#' This dataset includes data for gross inland consumption in Mtoe
#'
#' @return magpie object with OPENPROM input data iDataGrossInlCons
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataGrossInlCons", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join case_when if_else arrange
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom tibble deframe
#' @importFrom utils tail

calcIDataGrossInlCons <- function() {
  
  fuelMap <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, variable = OPEN.PROM)
  
  tes <- readSource("IEA2025", subset = "TES") %>%
    as.quitte() %>%
    filter(unit == "KTOE", product != "TOTAL", !is.na(value)) %>%
    select(-variable) %>%
    mutate(unit = "Mtoe", value = value / 1000) %>%
    inner_join(fuelMap, by = "product") %>%
    group_by(region, period, variable) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    as.quitte() %>%
    as.magpie() %>%
    # FIXME: Proper impute must be done. For now fill with zero.
    toolCountryFill(fill = 0)
  
  tes[is.na(tes)] <- 0
  
  # Set NA to 0
  tes[is.na(tes)] <- 0
  x <- tes
  
  list(x = x,
       weight = NULL,
       unit = "Mtoe",
       description = "IEA; Gross Inland Consumption")
}
