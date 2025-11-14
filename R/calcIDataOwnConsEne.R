#' calcIDataOwnConsEne
#'
#' Use data from IEA to derive OPENPROM input parameter IDataOwnConsEne
#' This dataset includes own consumption values for each region and energy branch in Mtoe.
#'
#' @return magpie object with OPENPROM input data IDataOwnConsEne
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataOwnConsEne", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate inner_join n
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom tidyr separate_rows

calcIDataOwnConsEne <- function() {
  fStartHorizon <- readEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fStartHorizon"]
  
  fuelMap2EFS <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, EFS = "OPEN.PROM")
  
  mapEF2EFS <- toolGetMapping(
    name = "prom-iea-ef.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows("IEAEF", sep = ",") %>%
    # Create shares for uniform disaggregation of EFs
    # FIXME: See FIXME bellow
    group_by(flow) %>%
    mutate(share = 1 / n()) %>%
    ungroup() %>%
    rename(EF = IEAEF)
  
  ownUseFlowPerEF <- readSource("IEA2025", subset = unique(mapEF2EFS$flow)) %>%
    as.quitte() %>%
    filter(unit == "KTOE", product != "TOTAL", !is.na(value)) %>%
    select(-variable) %>%
    mutate(unit = "Mtoe", value = -value / 1000) %>%
    inner_join(fuelMap2EFS, by = "product") %>%
    group_by(region, period, EFS, flow) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  
  fuelMap <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(EF = IEA, variable = OPEN.PROM)
  
  # Disaggregate own use sector of IEA to EF produced (e.g., EGASWKS -> ELC,STE)
  # FIXME: Disaggregation should be done based on fuel cons data, not uniformly
  totalOwnCons <- ownUseFlowPerEF %>%
    group_by(region, period, EFS) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    as.quitte() %>%
    as.magpie() %>%
    # FIXME: Proper impute must be done. For now fill with zero.
    toolCountryFill(fill = 0)
  
  totalOwnCons[is.na(totalOwnCons)] <- 0
  
  list(
    x = totalOwnCons,
    weight = NULL,
    unit = "Mtoe",
    description = "IEA; Own consumption of sectors"
  )
}