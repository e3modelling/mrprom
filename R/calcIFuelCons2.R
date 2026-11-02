#' calcIFuelCons2
#'
#' Imports and processes final fuel consumption data from the IEA 2025 database and
#' converts them into the OPEN-PROM input format. The IEA dataset provides historical
#' energy consumption data for 160 countries and 29 aggregate regions up to 2023.
#' Countries not covered by the IEA database are assigned zero values to ensure
#' complete geographical coverage across the 249 countries and regions represented
#' in the OPEN-PROM framework.
#' The function maps IEA products and flows to the corresponding OPEN-PROM energy
#' forms (EFs) and demand subsectors (DSBS). The OPEN-PROM framework distinguishes
#' four main end-use sectors: industry (INDSE), residential and commercial (DOMSE),
#' transport (TRANSE), and non-energy use and bunkers (NENSE). This sectoral
#' structure is used consistently across OPEN-PROM datasets to ensure harmonized
#' representation of fuel consumption, energy balances, and emissions.
#' To match the OPEN-PROM classification, several processing and disaggregation
#' steps are applied. First, non-energy use consumption is adjusted by separating
#' petrochemical feedstocks (PCH) from total non-energy use (NE_TOT), allowing the
#' residual non-energy demand category (NEN) to be derived. Fuel products are then
#' mapped from the IEA classification to OPEN-PROM energy forms using predefined
#' correspondence tables and aggregated to the required sectoral structure.
#' Transport fuel consumption is further disaggregated from aggregated IEA transport
#' categories into detailed transport subsectors, including passenger cars, public
#' buses, freight road transport, rail transport, domestic aviation, and domestic
#' navigation. The allocation shares are derived from Eurostat transport energy
#' statistics and are interpolated over time where required. These shares are
#' applied separately for each fuel type to preserve fuel-specific consumption
#' patterns across transport modes.
#' The resulting dataset provides fuel consumption by region, year, energy form,
#' and demand subsector in units of Mtoe and serves as an input dataset for the
#' OPEN-PROM modeling framework.
#'
#' @param subtype string, OPENPROM sector (DOMSE, INDSE, NENSE, TRANSE)
#' @return  OPENPROM input data iFuelConsXXX
#'
#' @author Michael Madianos, Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IFuelCons2", subtype = "DOMSE", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_rows crossing
#' @importFrom magclass as.magpie
#' @importFrom eurostat get_eurostat

calcIFuelCons2 <- function(subtype = "DOMSE") {
  EFtoEFS <- toolGetMapping(
    name = "EFtoEFS.csv",
    type = "blabla_export",
    where = "mrprom"
  ) %>%
    separate_rows(c("EF", "EFS"), sep = ",") %>%
    rename(ef = EF, efs= EFS)

  dataFuelCons <- calcOutput("IFinalEnergy", aggregate = FALSE) %>%
    as.quitte() %>%
    left_join(EFtoEFS, by = c("efs")) %>%
    group_by(region, period, dsbs, ef) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
}