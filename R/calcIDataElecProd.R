#' calcIDataElecProd
#'
#' Use ENERDATA electricity production data to derive OPENPROM input parameter iDataElecProd
#'
#' @return  OPENPROM input data iDataElecProd
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataElecProd", mode = "NonCHP", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select summarise left_join full_join right_join coalesce
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_rows

calcIDataElecProd <- function(mode = "NonCHP") {
  if (mode == "NonCHP") {
    subset <- c("ELMAINE", "ELAUTOE")
  } else if (mode == "CHP") {
    subset <- c("ELMAINC", "ELAUTOC")
  } else if (mode == "Total") {
    subset <- "ELOUTPUT"
  }
  fStartHorizon <- readEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fStartHorizon"]

  fuelMap <- toolGetMapping(
    name = "prom-iea-fuelcons-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, EF = OPEN.PROM)

  data <- readSource("IEA2025", subset = subset) %>%
    as.quitte() %>%
    filter(value != 0, !is.na(value), unit == "GWH") %>%
    mutate(unit = "GWh") %>%
    select(-variable) %>%
    # map IEA products to OPEN-PROM EFs
    inner_join(fuelMap, by = "product") %>%
    # Aggregate to OPEN-PROM's EFs & SBS
    group_by(region, period, EF) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

  if (mode %in% c("NonCHP", "Total")) {
    PGALLtoEF <- toolGetMapping(
      name = "PGALLtoEF.csv",
      type = "blabla_export",
      where = "mrprom"
    )
    capacities <- calcOutput(type = "IInstCapPast", mode = "Total", aggregate = FALSE)
    capacities <- add_columns(capacities, addnm=paste0("y",setdiff(unique(data[["period"]]),getYears(capacities, as.integer = TRUE))), dim = 2, fill = NA)
    # ENERDATA has years until 2021, interpolate until 2024 and use year only 2020 to take the shares
    capacities[,getYears(capacities),] <- capacities[,2020,]

    techProd <- left_join(data, PGALLtoEF, by = "EF", relationship = "many-to-many") %>%
      group_by(region, period, PGALL) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      left_join(helperGetSharesTech(capacities, data), by = c("region", "period", "PGALL")) %>%
      mutate(value = ifelse(is.na(share), 0, value * share)) %>%
      rename(variable = PGALL) %>%
      select(-share)
  } else if (mode == "CHP") {
    CHPtoEF <- toolGetMapping(
      name = "CHPtoEF.csv",
      type = "blabla_export",
      where = "mrprom"
    )
    techProd <- data %>%
      left_join(CHPtoEF, by = "EF") %>%
      group_by(region, period, CHP) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  }
  techProd <- techProd %>%
    # FIXME: NAs must be handled: e.g., HEAT must be distributed to the rest EFs
    drop_na() %>%
    as.quitte() %>%
    as.magpie()

  techProd[is.na(techProd)] <- 0
  techProd <- toolCountryFill(techProd, fill = 0)
  list(
    x = techProd,
    weight = NULL,
    unit = "GWh",
    description = "IEA; Electricity production"
  )
}

# Helper ------------------------------------------------
helperGetSharesTech <- function(capacities, techProd) {
  PGALLtoEF <- toolGetMapping(
    name = "PGALLtoEF.csv",
    type = "blabla_export",
    where = "mrprom"
  )

  shares <- capacities %>%
    as.quitte() %>%
    rename(PGALL = variable) %>%
    left_join(distinct(PGALLtoEF, PGALL, .keep_all = TRUE), by = "PGALL") %>%
    group_by(region, period, EF) %>%
    mutate(
      share = value / sum(value, na.rm = TRUE),
      share = ifelse(is.na(share), 1, share)
    ) %>%
    ungroup() %>%
    select(c("region", "period", "PGALL", "share"))
  return(shares)
}
