#' calcISuppTransfers
#'
#' Construct supplementary energy transfer parameters for OPEN-PROM
#' using IEA TRANSFERS data.
#'
#' This function derives the model input variable iSuppTransfers, which
#' represents energy transfers in Mtoe. Data are sourced from
#' readSource("IEA2025", subset = "TRANSFERS") and mapped from IEA
#' product definitions to OPEN-PROM variables using the mapping file
#' prom-iea-fuelcons-mapping.csv.
#'
#' Input data are originally reported in kilotonnes of oil equivalent (Ktoe)
#' and are converted to Mtoe by dividing by 1000.
#'
#' Only valid observations (unit == "KTOE", non-missing values, and
#' excluding totals) are retained. After mapping and aggregation across
#' regions, values are summed by region, year, and variable.
#'
#' Missing values are filled with zero using toolCountryFill(), and any
#' remaining NA values are explicitly set to zero as a final cleaning step.
#'
#' @return magpie object with OPENPROM input data iSuppTransfers.
#'
#' @author Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "ISuppTransfers", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_rows
calcISuppTransfers <- function() {
  fuelMap <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, variable = OPEN.PROM)

  # load TRANSFERS data from IEA
  data <- readSource("IEA2025", subset = "TRANSFERS") %>%
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

  # Set NA to 0
  data[is.na(data)] <- 0

  list(
    x = data,
    weight = NULL,
    unit = "Mtoe",
    description = "IEA; Transfers feedstock"
  )
}
