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
    inner_join(blend_map, by = "ef") %>%
    group_by(region, period, dsbs, blend) %>%
    mutate(
      blend_total = sum(value, na.rm = TRUE),
      value = value / blend_total,
      # If not fuel cons, put everything in fossil
      value = ifelse(is.na(value) & ef %in% c("GSL", "GDO", "KRS", "NGS"), 1, value),
      value = ifelse(is.na(value), 0, value)
    ) %>%
    ungroup() %>%
    select(region, period, dsbs, ef, blend, value)


    ######################################################################################


  # Add mandates by hand
  mandates <- share %>%
    group_by(region, dsbs, ef) %>%
    complete(region, dsbs, ef, period = 2024:2100) %>%
    mutate(
      value = ifelse(ef %in% c("OLQ", "RFO") & period >= 2024, value[period == 2023], value),
      value= if_else(region == "BRA" & ef %in% c("BGDO", "BGSL") & period == 2033, 1.5 * value[period == 2023], value),
      # value = if_else(region == "BRA" & ef %in% c("GSL", "GDO") & period == 2033, value[period == 2023], value),

      value = if_else(region == "ARG" & ef %in% c("BGDO", "BGSL") & period == 2026, 0.12, value),
      # value = if_else(region == "ARG" & ef %in% c("GDO", "GSL") & period == 2026, 0.12, value),
      value = if_else(region == "IND" & ef %in% c("BGSL") & period == 2027, 0.081, value),
      # value = if_else(region == "IND" & ef %in% c("GSL") & period == 2027, 1 - 0.081, value),

      # value = if_else(region == "BRA" & ef %in% c("GDO", "GSL") & period == 2033, 1 - 1.5 + 1.5 * value[period == 2023], value),
      value = zoo::na.approx(value, x = period, na.rm = FALSE, rule = 2)
    ) %>%
    ungroup()


  residual_map <- tibble(
    blend = c("GDO", "GSL"),
    residual_ef = c("GDO", "GSL")
  )

  share_final <- test %>%
    left_join(residual_map, by = "blend") %>%
    group_by(region, dsbs, blend, period) %>%
    mutate(
      fixed_sum = sum(
        value[ef != residual_ef],
        na.rm = TRUE
      ),
      value = if_else(
        ef == residual_ef,
        1 - fixed_sum, # residual absorbs the difference
        value
      )
    ) %>%
    ungroup() %>%
    select(-fixed_sum, -residual_ef)



  # Linear interpolation
  tt <- share_final %>%
    group_by(region, period, dsbs, blend) %>%
    mutate(sum = sum(value, na.rm = TRUE)) %>%
    ungroup()

  rename(variable = dsbs) %>%
    as.quitte() %>%
    as.magpie()

  weights <- transeFuel %>%
    inner_join(blend_map, by = "ef") %>%
    group_by(region, period, dsbs, blend) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    inner_join(blend_map, by = "blend", relationship = "many-to-many") %>%
    select(-blend) %>%
    rename(variable = dsbs) %>%
    as.quitte() %>%
    interpolate_missing_periods(period = 2010:2100, expand.values = TRUE) %>%
    as.magpie()
  weights <- weights[, , getItems(share, 3)] + 1e-6

  list(
    x = share,
    weight = weights,
    unit = "ratio",
    description = "Fuel blend ratio"
  )
}
