#' calcIDataAgricultureEff
#'
#' @return  Magpie object with the FAOProductionCrops
#'
#' @author Fotis Sioutas
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataAgricultureEff", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter left_join mutate select %>%
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom R.utils isZero

calcIDataAgricultureEff <- function() {
  extdata <- toolReadEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )
  AGRMODEStoTECH <- toolGetMapping("AGRMODEStoTECH.csv",
    type = "blabla_export",
    where = "mrprom",
  ) %>%
    separate_rows(AGRITECH, sep = ",")

  AGRITECHTOEF <- toolGetMapping("AGRITECHTOEF.csv",
    type = "blabla_export",
    where = "mrprom",
  ) %>%
    separate_rows(EF, sep = ",")

  TFC <- calcOutput(type = "IDataAgricultureTFC", aggregate = FALSE) %>%
    as.quitte() %>%
    right_join(AGRITECHTOEF, by = c("ef" = "EF"), relationship = "many-to-many") %>%
    group_by(region, period, variable, AGRITECH) %>%
    summarise(
      value = sum(value, na.rm = T), .groups = "drop"
    )
  SectorTFC <- TFC %>%
    group_by(region, period, variable) %>%
    summarise(
      value = sum(value, na.rm = T), .groups = "drop"
    )

  service <- calcOutput(type = "IDataAgricultureService", aggregate = FALSE) %>%
    as.quitte() %>%
    select(region, period, variable, value)

  dataGlobal <- service %>%
    left_join(SectorTFC, by = c("region", "period", "variable")) %>%
    group_by(period, variable) %>%
    summarise(
      service = sum(value.x, na.rm = TRUE),
      fuel = sum(value.y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(global = fuel / service) %>%
    select(period, variable, global)

  # eff_s where s is the sector and k the technology
  sectorEff <- service %>%
    left_join(SectorTFC, by = c("region", "period", "variable")) %>%
    group_by(region, period, variable) %>%
    summarise(
      service = sum(value.x, na.rm = TRUE),
      fuel = sum(value.y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(dataGlobal, by = c("period", "variable")) %>%
    ## filter(period <= extdata["fEndY"]) %>%
    mutate(
      value = fuel / service,
      value = ifelse(is.nan(value) | is.infinite(value) | is.na(value) | value == 0, global, value)
    ) %>%
    select(region, period, variable, value)

  # calculate eff_s,k where s is the sector and k the technology
  ## PLACEHOLDER. For now, put the same efficiency for all k.
  techEff <- expand.grid(
    region = unname(getISOlist()),
    period = seq(extdata["fStartHorizon"], extdata["fEndY"]),
    AGRI_MODES = unique(AGRMODEStoTECH$AGRI_MODES)
  ) %>%
    left_join(AGRMODEStoTECH, by = c("AGRI_MODES"), relationship = "many-to-many") %>%
    ## left_join(AGRITECHTOEF, by = c("AGRITECH"), relationship = "many-to-many") %>%
    mutate(value = 1) %>%
    rename(variable = AGRI_MODES)

  eff <- techEff %>%
    left_join(sectorEff, by = c("region", "period", "variable")) %>%
    mutate(value = value.x * value.y) %>%
    select(region, period, variable, AGRITECH, value) %>%
    as.quitte() %>%
    as.magpie()

  # --------------- Weights ---------------------------
  weights <- as.quitte(service) %>%
    filter(period <= extdata["fEndY"]) %>%
    as.magpie()

  list(
    x = eff,
    weight = weights,
    unit = "Mtoe / [Activity]",
    description = "FAOProductionCrops"
  )
}
