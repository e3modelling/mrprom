#' calcIDataIronSteelSFC
#'
#' @return  Magpie object
#'
#' @author Michael Madianos
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataIronSteelSFC", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter left_join mutate select %>% n_distinct
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIDataIronSteelSFC <- function() {
  extdata <- toolReadEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fEndY"]

  INDDOMtoSERVS <- toolGetMapping("INDDOMtoSERVS.csv",
    type = "blabla_export",
    where = "mrprom",
  ) %>%
    separate_rows(IND_SERVS, sep = ",")

  SERVStoTECH <- toolGetMapping("SERVStoTECH.csv",
    type = "blabla_export",
    where = "mrprom",
  ) %>%
    separate_rows(INDITECH, sep = ",")

  INDITECHTOEF <- toolGetMapping("INDITECHTOEF.csv",
    type = "blabla_export",
    where = "mrprom",
  ) %>%
    separate_rows(EF, sep = ",")

  # data for SFC shares :https://worldsteel.org/about-steel/energy-use-in-the-steel-industry/
  # data for SFC per tech: worldsteel
  # Error: Needs better approximation for the BOF SFC?? (Disaggregate from BF-BOF)
  techEff <- expand.grid(
    period = extdata,
    INDDOM = unique(INDDOMtoSERVS$INDDOM)
  ) %>%
    left_join(INDDOMtoSERVS, by = c("INDDOM"), relationship = "many-to-many") %>%
    left_join(SERVStoTECH, by = c("IND_SERVS"), relationship = "many-to-many") %>%
    left_join(INDITECHTOEF, by = c("INDITECH"), relationship = "many-to-many") %>%
    group_by(INDDOM, IND_SERVS, INDITECH) %>%
    mutate(n_fuels = n_distinct(EF)) %>%
    ungroup() %>%
    mutate(
      value = 0,
      value = case_when(
        INDDOM == "IS" & IND_SERVS == "PRIM_STEEL" & INDITECH == "EAF" & EF == "ELC" ~ 0.4 * 0.07739, #units mtoe/Million tonnes
        INDDOM == "IS" & IND_SERVS == "PRIM_STEEL" & INDITECH == "EAF" & !(EF == "ELC") ~ 0.6 * 0.07739, #/ (n_fuels-1) OLDER VERSION
        INDDOM == "IS" & IND_SERVS == "PRIM_STEEL" & INDITECH == "BOF" & EF == "ELC" ~ 0.25 * 0.0065, #1.2GJ/t=0.0287 is an approximation
        INDDOM == "IS" & IND_SERVS == "PRIM_STEEL" & INDITECH == "BOF" & !(EF == "ELC") ~ 0.75 * 0.0065, #1.2GJ/t=0.0287 is an approximation
        INDDOM == "IS" & IND_SERVS == "SEC_STEEL" & INDITECH == "EAF" & EF == "ELC" ~ 0.85 * 0.06759,
        INDDOM == "IS" & IND_SERVS == "SEC_STEEL" & INDITECH == "EAF" & !(EF == "ELC") ~ 0.25 * 0.06759, # CHECK IF EAF HAS THERMAL INPUT
        !(INDDOM == "IS") ~ 1,
        TRUE ~ value
      )
    ) %>%
    rename(variable = INDDOM) %>%
    select(-n_fuels) %>%
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
