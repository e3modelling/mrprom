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
        INDDOM == "IS" & IND_SERVS == "PRIM_STEEL" & INDITECH == "EAF" & EF == "ELC" ~ 0.3 * 17.1,
        INDDOM == "IS" & IND_SERVS == "PRIM_STEEL" & INDITECH == "EAF" & !(EF == "ELC") ~ 0.7 / (n_fuels-1) * 17.1,
        INDDOM == "IS" & IND_SERVS == "PRIM_STEEL" & INDITECH == "BOF" & EF == "ELC" ~ 0.12 * 1.2,
        INDDOM == "IS" & IND_SERVS == "PRIM_STEEL" & INDITECH == "BOF" & !(EF == "ELC") ~ 0.88 / (n_fuels-1) * 1.2,
        INDDOM == "IS" & IND_SERVS == "SEC_STEEL" & INDITECH == "EAF" & EF == "ELC" ~ 0.6 * 2.1,
        INDDOM == "IS" & IND_SERVS == "SEC_STEEL" & INDITECH == "EAF" & !(EF == "ELC") ~ 0.4 / (n_fuels-1) * 2.1,
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
