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
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  # Add new own energy use: DRI
  SteelToFuelDRI <- crossing(
    region = unname(getISOlist()),
    inditech = c("EAF"),
    tibble(
      ef = c("NGS", "HCL", "ELC"),
      value = c(0.263 * 1.1, 0, 0.0095 * 1.1)
    )
  ) %>%
    mutate(
      value = case_when(
        region %in% c("IND") & ef == "NGS" ~ 0,
        region %in% c("IND") & ef == "HCL" ~ 0.263 * 1.1,
        TRUE ~ value
      )
    )

  DRI <- calcOutput("iProductionIS", subtype = "IS", aggregate = FALSE) %>%
    as.quitte() %>%
    filter(
      dsbs == "IS",
      ind_servs == "PRIM_STEEL",
      inditech == "EAF"
    ) %>%
    left_join(SteelToFuelDRI, by = c("region", "inditech"), relationship = "many-to-many") %>%
    mutate(
      value = value.x * value.y,
      flow = "DRI"
    ) %>%
    select(region, period, flow, ef, value) %>%
    rename(variable = ef)

  ownEnergy <- ownEnergy %>%
    rbind(DRI)
  #  --------------------------------------

  ownEnergy <- ownEnergy %>%
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
