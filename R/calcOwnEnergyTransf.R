#' calcOwnEnergyTransf
#'
#' @author Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "OwnEnergyTransf", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie

calcOwnEnergyTransf <- function() {
  AllFlows <- c("EBLASTFUR", "ECOKEOVS")

  fuelMap <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, variable = OPEN.PROM)

  ownEnergy <- readSource("IEA2025", subset = AllFlows) %>%
    as.quitte() %>%
    filter(
      unit == "KTOE",
      product != "TOTAL",
      !is.na(value)
    ) %>%
    select(-variable) %>%
    mutate(unit = "Mtoe", value = -value / 1000)
  ownEnergy$flow <- substring(ownEnergy$flow, 2)

  ownEnergy <- ownEnergy %>%
    inner_join(fuelMap, by = "product") %>%
    group_by(region, period, variable, flow) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    as.quitte() %>%
    as.magpie() %>%
    # FIXME: Proper impute must be done. For now fill with zero.
    toolCountryFill(fill = 0)

  ownEnergy[is.na(ownEnergy)] <- 0

  list(
    x = ownEnergy,
    weight = NULL,
    unit = "Mtoe",
    description = "IEA; Own energy use for transformation"
  )
}
