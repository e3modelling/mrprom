#' calciProductionIS
#'
#'
#' @return magpie object with OPENPROM input data iProductionIS
#'
#' @author Margarita Efthymiadou
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "iProductionIS", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% mutate
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @importFrom madrat toolGetMapping
#' @importFrom tidyr crossing
calciProductionIS <- function() {
  # --- read data ---
  IS <- readSource("WSA_Prod_DRI") %>%
    as.quitte() %>%
    select(-c("model", "scenario", "unit")) %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    mutate(
      # value = ifelse(is.na(value), 0, value), # !ERROR: NEEDS double check
      BOF = Total_Production * Oxygen / 100,
      EAF = Total_Production * Electric / 100,
      other = Total_Production * (Open_hearth + Other) / 100,
      DRI = DRI_Production * ifelse(region %in% c("IRN", "LBY"), 0.7, 0.9),
      SCREAF = EAF - DRI
    ) %>%
    select(region, period, BOF, DRI, SCREAF, other) %>%
    pivot_longer(
      cols = c("BOF", "DRI", "SCREAF", "other"),
      names_to = "variable", values_to = "value"
    ) %>%
    mutate(value = pmax(replace_na(value, 0), 0))

  extdata <- toolReadEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )

  prod <- IS %>%
    filter(!(variable == "other")) %>%
    rename(inditech = variable) %>%
    mutate(
      dsbs = "IS",
      ind_servs = ifelse(inditech == "SCREAF", "SEC_STEEL", "PRIM_STEEL"),
      inditech = ifelse(inditech == "SCREAF", "EAF", inditech),
      inditech = ifelse(inditech == "DRI", "EAF", inditech)
    )

  INDDOMtoSERVS <- toolGetMapping("INDDOMtoSERVS.csv",
    type = "blabla_export",
    where = "mrprom",
  ) %>%
    separate_rows(IND_SERVS, sep = ",") %>%
    filter(!(INDDOM == "IS"))

  SERVStoTECH <- toolGetMapping("SERVStoTECH.csv",
    type = "blabla_export",
    where = "mrprom",
  ) %>%
    separate_rows(INDITECH, sep = ",")

  REST <- expand.grid(
    region = unname(getISOlist()),
    period = seq(extdata["fStartHorizon"], extdata["fEndY"]),
    INDDOM = unique(INDDOMtoSERVS$INDDOM)
  ) %>%
    left_join(INDDOMtoSERVS, by = c("INDDOM"), relationship = "many-to-many") %>%
    left_join(SERVStoTECH, by = c("IND_SERVS"), relationship = "many-to-many") %>%
    group_by(region, period, INDDOM, IND_SERVS) %>%
    mutate(value = 1 / n()) %>%
    ungroup() %>%
    rename(dsbs = INDDOM, ind_servs = IND_SERVS, inditech = INDITECH)

  activity <- prod %>%
    rbind(REST) %>%
    as.quitte() %>%
    as.magpie()
  
  activity[is.na(activity)] <- 0
  list(
    x = activity,
    weight = NULL,
    unit = "various",
    description = "Historical production BFBOF, DREAF and SCREAF in IS route"
  )
}
