#' calcIDataShareFuel
#'
#' Derive the OPENPROM input parameter IDataShareFuel for transport
#' fuel blending shares by demand subsector and country.
#' The dataset contains historical and projected shares of fossil and blended
#' fuels, expressed as ratios within each fuel blend category.
#' Historical blending shares are calculated from transport fuel consumption
#' data and mapped to aggregate blend categories.
#' Country-specific blending mandates and assumptions are applied for selected
#' regions and fuels, with missing years linearly interpolated over time.
#' Missing values are replaced with zero where no blending activity is
#' reported or assumed.
#'
#' @return magpie object
#'
#' @author Michael Madianos
#'
#' @examples
#' \dontrun{
#' shareFuel <- calcOutput("IDataShareFuel", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>%  mutate rename select group_by summarise ungroup inner_join left_join bind_rows
#' @importFrom tidyr replace_na
#' @importFrom magclass as.magpie
#' @importFrom quitte as.quitte

calcIDataShareFuel <- function() {
  TECHtoEF <- toolGetMapping(
    name = "TTECHtoEF.csv",
    type = "blabla_export",
    where = "mrprom") %>%
    separate_rows(EF, sep = ",")
  
  SECTTECH <- toolGetMapping(
    name = "SECTTECH.csv",
    type = "blabla_export",
    where = "mrprom") %>%
    separate_rows(c("TECH"), sep = ",") %>%
    separate_rows(c("DSBS"), sep = ",")

  shares <- SECTTECH %>%
    filter(DSBS %in% c("PC", "PB", "PT", "GU", "GT", "GN", "PA", "PN")) %>%
    left_join(TECHtoEF, by = c("TECH" = "TTECH"), relationship = "many-to-many") %>%
    mutate(value = ifelse(!(TECH %in% c("TPHEVGSL", "TPHEVGDO")), 1, 0.5)) %>%
    as.quitte() %>%
    as.magpie()

  ######################################################################################

  # mandates <- share %>%
  #   complete(
  #     nesting(region, dsbs, efs),
  #     period = 2010:2100
  #   ) %>%
  #   group_by(region, dsbs, efs) %>%

  #   mutate(
  #     value = ifelse(efs %in% c("OLQ", "RFO") & period >= 2024, value[period == 2023], value),
  #     value = ifelse(
  #       region == "BRA" & dsbs %in% c("PC", "PB", "GU") & efs %in% c("BGDO", "BGSL") & period == 2033,
  #       1.5 * value[period == 2023], value
  #     ),
  #     value = ifelse(
  #       region == "BRA" & dsbs %in% c("PC", "PB", "GU") & efs %in% c("FGDO", "FGSL") & period == 2033,
  #       1 - 1.5 + 1.5 * value[period == 2023], value
  #     ),
  #     value = ifelse(region == "ARG" & dsbs %in% c("PC", "PB", "GU") & efs %in% c("BGDO", "BGSL") & period == 2026, 0.12, value),
  #     value = ifelse(region == "ARG" & dsbs %in% c("PC", "PB", "GU") & efs %in% c("FGDO", "FGSL") & period == 2026, 0.12, value),
  #     value = ifelse(region == "IND" & dsbs %in% c("PC", "PB", "GU") & efs %in% c("BGSL") & period == 2027, 0.081, value),
  #     value = ifelse(region == "IND" & dsbs %in% c("PC", "PB", "GU") & efs %in% c("FGSL") & period == 2027, 1 - 0.081, value),
  #     # Linear interpolation
  #     value = zoo::na.approx(value, x = period, na.rm = FALSE, rule = 2),
  #     # Replace na = 0
  #     value = ifelse(is.na(value), 0, value)
  #   ) %>%
  #   ungroup() %>%
  #   as.quitte() %>%
  #   as.magpie()

  # mandates[is.na(mandates)] <- 0

  weights <- shares
  weights[, , ] <- 1

  # map <- toolGetMapping("regionmappingOPDEV3.csv", "regional", where = "mrprom")
  # a <- toolAggregate(mandates, weight = NULL, rel = map, from = "ISO3.Code", to = "Region.Code")
  list(
    x = shares,
    weight = weights,
    unit = "ratio",
    description = "Fuel share"
  )
}
