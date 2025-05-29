#' calcIDataElecSteamGen
#'
#' Use data to derive OPENPROM input parameter iDataElecSteamGen
#'
#' @return  OPENPROM input data iDataElecSteamGen
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataElecSteamGen", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join case_when if_else arrange
#' @importFrom tidyr pivot_wider spread gather
#' @importFrom quitte as.quitte

calcIDataElecSteamGen <- function() {
  EffCapacities <- calcOutput(type = "IInstCapPast", aggregate = FALSE) %>%
    as.quitte()
  avail_rates <- calcOutput(
    type = "IAvailRate", aggregate = FALSE
  )["GLO", "y2020", ] %>%
    as.quitte() %>%
    rename(avail = value) %>%
    select(c("variable", "avail"))

  NominalCapacities <- EffCapacities %>%
    left_join(avail_rates, by = "variable") %>%
    mutate(value = value / avail)

  TOTINSTCAP <- readSource("ENERDATA", "capacity", convert = TRUE) %>%
    as.quitte() %>%
    filter(variable == "Installed electricity capacity") %>%
    select(c("region", "period", "value")) %>%
    mutate(value = value / 1000) # converting MW values to GW

  TOTNOMCAP <- NominalCapacities %>%
    mutate(TOTNOMCAP_value = sum(value, na.rm = TRUE), .by = c("region", "period")) %>%
    select(c("region", "period", "TOTNOMCAP_value")) %>%
    unique() %>%
    left_join(TOTINSTCAP, by = c("region", "period")) %>%
    mutate(TOTNOMCAP = ifelse(is.na(value), TOTNOMCAP_value, value)) %>%
    select(-c("TOTNOMCAP_value", "value"))

  ElecSteamGen <- NominalCapacities %>%
    mutate(TOTCAP = sum(value * avail, na.rm = TRUE), .by = c("region", "period")) %>%
    select(c("region", "period", "TOTCAP")) %>%
    left_join(TOTNOMCAP, by = c("region", "period")) %>%
    unique() %>%
    mutate(
      PEAKLOAD = TOTCAP * 0.9,
      BASELOAD = PEAKLOAD * 0.3576,
      Non_CHP_Per = 0.00000001,
      CHP_Cap = 0.00000001,
      CHP_ELC = 0.00000001,
      # STE1CL = 0.0, STE1CH = 0.0, STE1CD = 0.0,
      # STE1CR = 0.0, STE1CG = 0.0, STE1CB = 0.0,
      STE1AL = 0.0, STE1AH = 0.0, STE1AD = 0.0,
      STE1AR = 0.0, STE1AG = 0.0, STE1AB = 0.0,
      STE1AH2F = 0.0
    ) %>%
    pivot_longer(cols = -c("region", "period"), names_to = "variable", values_to = "value") %>%
    as.quitte() %>%
    as.magpie()

  list(
    x = ElecSteamGen,
    weight = NULL,
    unit = "GW",
    description = "Enerdata; Installed capacity"
  )
}
