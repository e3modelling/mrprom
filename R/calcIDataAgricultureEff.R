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

  # calculate eff_s,k where s is the sector and k the technology
  ## PLACEHOLDER. For now, put the same efficiency for all k.
  techEff <- expand.grid(
    region = unname(getISOlist()),
    period = seq(extdata["fStartHorizon"], extdata["fEndY"]),
    AGRI_MODES = unique(AGRMODEStoTECH$AGRI_MODES)
  ) %>%
    left_join(AGRMODEStoTECH, by = c("AGRI_MODES"), relationship = "many-to-many") %>%
    ## left_join(AGRITECHTOEF, by = c("AGRITECH"), relationship = "many-to-many") %>%
    mutate(
      value = ifelse(AGRITECH == "TELC", 0.7, 1)
    ) %>%
    rename(variable = AGRI_MODES) %>%
    as.quitte() %>%
    as.magpie()
  # --------------- Weights ---------------------------
  weights <- techEff
  weights[,,] <- 1

  list(
    x = techEff,
    weight = weights,
    unit = "Mtoe / [Activity]",
    description = "efficiency [Mtoe / Activity]"
  )
}
