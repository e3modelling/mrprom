#' calcIDataGrossInlCons
#'
#' Use IEA data to derive OPENPROM input parameter IDataGrossInlCons
#'
#' @return  OPENPROM input data
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataGrossInlCons", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom quitte as.quitte

calcIDataGrossInlCons <- function() {
  fuelCons <- calcOutput(type = "IFuelCons2", aggregate = FALSE) %>%
    as.quitte() %>%
    filter(dsbs != "BU") %>%
    group_by(region, period, ef) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    rename(cons = value, variable = ef) %>%
    select(region, period, variable, cons)

  distrLosses <- calcOutput(type = "IDataDistrLosses", aggregate = FALSE) %>%
    as.quitte() %>%
    select(region, period, variable, value) %>%
    rename(losses = value)

  transfers <- calcOutput(type = "ISuppTransfers", aggregate = FALSE) %>%
    as.quitte() %>%
    select(region, period, variable, value) %>%
    rename(transfers = value)

  ownCons <- calcOutput("IDataOwnConsEne", aggregate = FALSE) %>%
    as.quitte() %>%
    select(region, period, sector, efs, value) %>%
    rename(ownCons = value, variable = efs)

  transfProcess <- calcOutput(type = "ITransfProcess", aggregate = FALSE) %>%
    as.quitte() %>%
    select(region, period, variable, sector, value) %>%
    rename(transformation = value)

  # Aggregate for all SBS for variables that has this granularity
  temp <- ownCons %>%
    full_join(transfProcess, by = c("region", "period", "variable", "sector")) %>%
    mutate(
      ownCons = ifelse(is.na(ownCons), 0, ownCons),
      transformation = ifelse(is.na(transformation), 0, transformation),
      temp = ownCons - transformation
    ) %>%
    group_by(region, period, variable) %>%
    summarise(temp = sum(temp, na.rm = TRUE), .groups = "drop")

  data <- distrLosses %>%
    full_join(fuelCons, by = c("region", "period", "variable")) %>%
    mutate(cons = ifelse(is.na(cons), 0, cons)) %>%
    full_join(transfers, by = c("region", "period", "variable")) %>%
    full_join(temp, by = c("region", "period", "variable")) %>%
    mutate(value = temp + cons + losses - transfers) %>%
    select(region, period, variable, value) %>%
    as.quitte() %>%
    as.magpie() %>%
    # FIXME: Proper impute must be done. For now fill with zero.
    toolCountryFill(fill = 0)

  data[is.na(data)] <- 0

  list(
    x = data,
    weight = NULL,
    unit = "Mtoe",
    description = "IEA; Gross Inland Consumption"
  )
}
