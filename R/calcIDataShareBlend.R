#' calcIDataShareBlend
#'
#' Derive the fuel blend per demand subsectors for all countries
#'
#' @return magpie object
#'
#' @author Michael Madianos
#'
#' @examples
#' \dontrun{
#' shareBlend <- calcOutput("IDataShareBlend", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>%  mutate rename select group_by summarise ungroup inner_join left_join bind_rows
#' @importFrom tidyr replace_na
#' @importFrom magclass as.magpie
#' @importFrom quitte as.quitte

calcIDataShareBlend <- function() {
  blend_map <- list(
    GDO = c("GDO", "BGDO", "OLQ", "RFO"),
    GSL = c("GSL", "BGSL"),
    KRS = c("KRS", "BKRS"),
    NGS = c("NGS", "OGS")
  ) %>%
    stack() %>%
    rename(ef = values, blend = ind)

  transeFuel <- calcOutput(
    type = "IFuelCons2",
    subtype = "TRANSE",
    aggregate = FALSE
  ) %>%
    as.quitte() %>%
    filter(ef %in% blend_map$ef)

  # Historical shares
  share <- transeFuel %>%
    left_join(blend_map, by = "ef") %>%
    group_by(region, period, dsbs, blend) %>%
    mutate(
      blend_total = sum(value, na.rm = TRUE),
      value = value / blend_total,
      # If not fuel cons, put everything in fossil
      value = ifelse(is.na(value) & ef %in% c("GSL", "GDO", "KRS", "NGS"), 1, value),
      value = ifelse(is.na(value), 0, value)
    ) %>%
    ungroup() %>%
    select(region, period, dsbs, ef, value)


  ######################################################################################

  mandates <- share %>%
    complete(
      nesting(region, dsbs, ef),
      period = 2010:2100
    ) %>%
    group_by(region, dsbs, ef) %>%
    mutate(
      value = ifelse(ef %in% c("OLQ", "RFO") & period >= 2024, value[period == 2023], value),
      value = ifelse(
        region == "BRA" & dsbs %in% c("PC", "PB", "GU") & ef %in% c("BGDO", "BGSL") & period == 2033,
        1.5 * value[period == 2023], value
      ),
      value = ifelse(
        region == "BRA" & dsbs %in% c("PC", "PB", "GU") & ef %in% c("GDO", "GSL") & period == 2033,
        1 - 1.5 + 1.5 * value[period == 2023], value
      ),
      value = ifelse(region == "ARG" & dsbs %in% c("PC", "PB", "GU") & ef %in% c("BGDO", "BGSL") & period == 2026, 0.12, value),
      value = ifelse(region == "ARG" & dsbs %in% c("PC", "PB", "GU") & ef %in% c("GDO", "GSL") & period == 2026, 0.12, value),
      value = ifelse(region == "IND" & dsbs %in% c("PC", "PB", "GU") & ef %in% c("BGSL") & period == 2027, 0.081, value),
      value = ifelse(region == "IND" & dsbs %in% c("PC", "PB", "GU") & ef %in% c("GSL") & period == 2027, 1 - 0.081, value),
      # Linear interpolation
      value = zoo::na.approx(value, x = period, na.rm = FALSE, rule = 2),
      # Replace na = 0
      value = ifelse(is.na(value), 0, value)
    ) %>%
    ungroup() %>%
    as.quitte() %>%
    as.magpie()

  mandates[is.na(mandates)] <- 0

  weights <- transeFuel %>%
    left_join(blend_map, by = "ef") %>%
    group_by(region, period, dsbs, blend) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    left_join(blend_map, by = "blend", relationship = "many-to-many") %>%
    select(-blend) %>%
    rename(variable = dsbs) %>%
    as.quitte() %>%
    interpolate_missing_periods(period = 2010:2100, expand.values = TRUE) %>%
    as.magpie()

  # weights <- add_columns(weights, addnm = setdiff(getItems(mandates, 3), getItems(weights, 3)), dim = 3, fill = 0)
  weights <- mandates
  weights[, , ] <- 1

  # map <- toolGetMapping("regionmappingOPDEV3.csv", "regional", where = "mrprom")
  # a <- toolAggregate(mandates, weight = NULL, rel = map, from = "ISO3.Code", to = "Region.Code")
  list(
    x = mandates,
    weight = weights,
    unit = "ratio",
    description = "Fuel blend ratio"
  )
}
