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
    left_join(AGRITECHTOEF, by = c("AGRITECH"), relationship = "many-to-many") %>%
    mutate(
      value = case_when(
        AGRI_MODES == "IRRIGATION" & AGRITECH == "TELC" ~ 260, # References: 10.1029/2025EF007970, 10.1039/d2va00126h
        AGRI_MODES == "IRRIGATION" & AGRITECH == "TGDO" ~ 260 * 3,
        AGRI_MODES == "IRRIGATION" & AGRITECH == "TSOL" ~ 260,
        AGRI_MODES == "FORESTRY" & AGRITECH == "TGDO" ~ 0.0019021, # Reference https://doi.org/10.1007/s40725-024-00219-3
        AGRI_MODES == "FORESTRY" & AGRITECH == "TGDO" ~ 0.0019021 * 0.7,
        TRUE ~ 1
      )
    ) %>%
    rename(variable = AGRI_MODES) %>%
    as.quitte() %>%
    as.magpie()
  # --------------- Weights ---------------------------
  weights <- techEff
  weights[, , ] <- 1

  list(
    x = techEff,
    weight = weights,
    unit = "Mtoe / [Activity]",
    description = "efficiency [Mtoe / Activity]"
  )
}

# ----------- Helper --------------------------------------
