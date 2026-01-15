#' calcIRatioBranchOwnCons
#'
#' Use ...
#' to derive OPENPROM input parameter iRatioBranchOwnCons.
#'
#' @return  OPENPROM input data iRatioBranchOwnCons.
#' The output data calculated from the IEA.
#'
#' @author Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IRatioBranchOwnCons", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>% mutate filter select distinct left_join
#' @importFrom tidyr drop_na nesting expand complete
#'
calcIRatioBranchOwnCons <- function() {
  SECtoEFPROD <- toolGetMapping(
    name = "SECtoEFPROD.csv",
    type = "blabla_export",
    where = "mrprom"
  ) %>%
    separate_rows(EFS, sep = ",") %>%
    rename(variable = EFS, sector = SSBS)

  PGRENEF <- toolGetMapping(
    name = "PGRENEF.csv",
    type = "blabla_export",
    where = "mrprom"
  )$PGRENEF

  ownConsumption <- calcOutput(type = "IDataOwnConsEne", aggregate = FALSE) %>%
    as.quitte() %>%
    select(region, period, sector, efs, value) %>%
    rename(variable = efs)

  primary <- calcOutput(
    type = "ITotEneSupply",
    subtype = "Primary",
    aggregate = FALSE
  ) %>%
    as.quitte() %>%
    filter(!(variable %in% PGRENEF)) %>%
    left_join(SECtoEFPROD, by = "variable") %>%
    rename(primary = value) %>%
    select(region, period, sector, variable, primary)

  transfProcessOut <- calcOutput(
    type = "ITransfProcess",
    flow = "Out",
    aggregate = FALSE
  ) %>%
    as.quitte()

  totalProduction <- transfProcessOut %>%
    full_join(primary, by = c("region", "period", "sector", "variable")) %>%
    replace_na(list(value = 0, primary = 0)) %>%
    mutate(value = value + primary)  %>%
    group_by(region, period, sector) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  GlobalAvg <- helperGenerateAvg(ownConsumption, totalProduction)

  branchRatio <- ownConsumption %>%
    full_join(totalProduction, by = c("region", "period", "sector")) %>%
    mutate(value = value.x / value.y) %>%
    select(-c("value.x", "value.y")) %>%
    left_join(GlobalAvg, by = c("period", "sector", "variable")) %>%
    mutate(value = ifelse(is.na(value.x) | is.infinite(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y")) %>%
    as.quitte() %>%
    as.magpie()

  weights <- totalProduction %>%
    mutate(value = ifelse(value == 0, 1e-6, value)) %>%
    as.quitte() %>%
    as.magpie()
  list(
    x = branchRatio,
    weight = weights,
    unit = "Mtoe",
    description = "IEA; Transformation Processes"
  )
}

# Helper --------------------------------------------------------------------------------
helperGenerateAvg <- function(ownConsumption, totalProduction) {
  # Get the global avg of branching ratio and fill H2P's ELC own use with PG's
  avg <- ownConsumption %>%
    full_join(totalProduction, by = c("region", "period", "sector")) %>%
    group_by(period, sector, variable) %>%
    summarise(
      value.x = sum(value.x, na.rm = TRUE),
      value.y = sum(value.y, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(value = value.x / value.y) %>%
    select(-c("value.x", "value.y"))

  avg <- avg %>%
    left_join(
      filter(avg, sector == "PG") %>% select(-sector),
      by = c("period", "variable")
    ) %>%
    mutate(value = ifelse(sector == "H2P" & variable == "ELC", value.y, value.x)) %>%
    select(period, sector, variable, value)
}
