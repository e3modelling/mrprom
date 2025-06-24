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

calcIDataElecProd <- function(mode) {
  capacities <- calcOutput(type = "IInstCapPast", mode = "Total", aggregate = FALSE)
  if (mode == "NonCHP") {
    data <- readSource("IEA2024", subtype = "ELMAINE") +
      readSource("IEA2024", subtype = "ELAUTOE")
  } else if (mode == "CHP") {
    data <- readSource("IEA2024", subtype = "ELMAINC") +
      readSource("IEA2024", subtype = "ELAUTOC")
  } else if (mode == "Total") {
    data <- readSource("IEA2024", subtype = "ELOUTPUT")
  }
  
  data <- collapseDim(data, 3.4)
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  years <- getYears(data, as.integer = TRUE)
  data <- as.quitte(data) %>%
    filter(period >= fStartHorizon & period <= 2021) %>%
    replace_na(list(value = 0))

  # load current OPENPROM set configuration
  sets <- toolGetMapping(
    name = "PGALL.csv",
    type = "blabla_export",
    where = "mrprom"
  )[, 1]

  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(
    name = "prom-iea-elecprod-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  ) %>%
    filter(PGALL %in% sets) %>%
    separate_rows(IEA, sep = ",") %>%
    rename(product = IEA, variable = PGALL)

  # group by each technology and sum over its sub-technologies
  techProd <- data %>%
    left_join(map, by = "product", relationship = "many-to-many") %>%
    rename(variable = "variable.y") %>%
    select(c("region", "period", "value", "variable")) %>%
    group_by(region, period, variable) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    drop_na()

  if (mode %in% c("NonCHP", "Total")) {
    shares <- Reduce(
      function(x, y) full_join(x, y, by = c("region", "period", "variable")),
      list(
        getSharesTech(capacities, techProd, c("PGSOL", "PGCSP")),
        getSharesTech(capacities, techProd, c("PGLHYD", "PGSHYD")),
        getSharesTech(capacities, techProd, c("PGAWND", "PGAWNO"))
      )
    ) %>%
      mutate(value = coalesce(value.y, value.x, value)) %>%
      select(region, period, variable, value)

    techProd <- techProd %>%
      left_join(shares, by = c("region", "variable", "period")) %>%
      mutate(value = ifelse(is.na(value.y), value.x, value.y)) %>%
      select(c("region", "period", "variable", "value"))
  } else if (mode == "CHP") {
    CHPtoEF <- toolGetMapping(
      name = "CHPtoEF.csv",
      type = "blabla_export",
      where = "mrprom"
    )
    mapping <- setNames(CHPtoEF$CHP, CHPtoEF$EF)
    techProd$variable <- mapping[techProd$variable]
    techProd <- drop_na(techProd)
  }
  techProd <- as.quitte(techProd) %>% as.magpie()

  list(
    x = collapseNames(techProd),
    weight = NULL,
    unit = "GWh",
    description = "Enerdata; Electricity production"
  )
}

# Helper ------------------------------------------------
getSharesTech <- function(capacities, techProd, vars) {
  shares <- capacities %>%
    as.quitte() %>%
    filter(variable %in% vars) %>%
    group_by(region, period) %>% # Group by region and period
    mutate(total_value = sum(value), share = value / total_value) %>%
    select(c("region", "variable", "period", "share")) %>%
    replace_na(list(share = 0)) %>%
    right_join(techProd, by = c("region", "period", "variable")) %>%
    mutate(value = value * share) %>%
    select(-share)
  return(shares)
}

disaggregateTechs <- function(techProd, vars) {
  z <- techProd %>%
    left_join(getSharesTech(capacities, z, vars),
      by = c("region", "period", "variable")
    ) %>%
    mutate(value = ifelse(is.na(value.y), value.x, value.y)) %>%
    select(c("region", "period", "variable", "value"))
  return(z)
}
