#' calcIDataElecProd
#'
#' Creates the OPEN-PROM input parameter {iDataElecProd}, which represents
#' historical electricity generation by technology, region, and year. The dataset
#' is derived from the IEA 2025 electricity production database, which provides
#' electricity generation statistics for 169 countries and 17 aggregate regions up
#' to 2023. To ensure complete geographical coverage across the 249-country
#' structure used by OPEN-PROM, countries without available source data are
#' assigned zero values.
#'
#' The function supports three calculation modes: {NonCHP},
#' {CHP}, and {Total}. Depending on the selected mode, different IEA
#' electricity production subsets are extracted. The {NonCHP} mode uses the
#' {ELMAINE} and {ELAUTOE} subsets, which represent electricity
#' generation excluding combined heat and power (CHP) plants. The {CHP} mode
#' uses the {ELMAINC} and {ELAUTOC} subsets, which contain electricity
#' production from CHP facilities. The {Total} mode uses the broader
#' {ELOUTPUT} subset, providing total electricity generation without
#' distinguishing between CHP and non-CHP technologies.
#'
#' Electricity production data are first mapped from IEA fuel products to the
#' corresponding OPEN-PROM energy forms (EFs) using predefined correspondence
#' tables. Production values are then aggregated by region, year, and energy form.
#' For the {NonCHP} and {Total} modes, electricity generation is further
#' disaggregated from energy forms to individual power generation technologies
#' (PGALL) using technology-specific installed-capacity shares derived from
#' {IInstCapPast}. Technologies represented by multiple OPEN-PROM categories within the same
#' energy form, generation is allocated using ENERDATA-based installed-capacity
#' shares. This includes the disaggregation of solar generation into
#' {PGSOL} and {PGCSP}, hydropower generation into {PGLHYD} and
#' {PGSHYD}, and wind generation into {PGAWND} and {PGAWNO}.
#'
#' For the {CHP} mode, electricity generation is allocated directly to CHP
#' technologies using a dedicated CHP-to-energy-form mapping. Production values are
#' aggregated by technology after the mapping step, preserving the CHP-specific
#' technology structure.
#'
#' Missing values are replaced with zero, and the resulting dataset is expanded to
#' provide complete country coverage across the OPEN-PROM model domain. The final
#' output contains electricity generation by region, year, and generation
#' technology expressed in GWh and serves as an input dataset for the OPEN-PROM
#' modeling framework.
#'
#' @param mode Character string specifying the electricity production mode.
#' Supported values are "NonCHP","CHP" and "Total".
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
  ) %>%
    separate_rows(EF, sep = ",") %>%
    rename(variable = CHP)

    techProd <- data %>%
      left_join(CHPtoEF, by = "EF") %>%
      group_by(region, period, variable) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  }
  techProd <- techProd %>%
    # FIXME: NAs must be handled: e.g., HEAT must be distributed to the rest EFs
    drop_na() %>%
    as.quitte() %>%
    as.magpie()

  techProd[is.na(techProd)] <- 0
  
  suppressMessages(
    suppressWarnings(
      techProd <- toolCountryFill(techProd, fill = 0)
    )
  )

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
  disaggregatetechs <- c("PGSOL","PGCSP","PGLHYD","PGSHYD","PGAWND", "PGAWNO")
  
  capacities <- add_columns(capacities, addnm = c("y2022","y2023"), dim = 2, fill = NA)
  capacities[,c(2021,2022,2023),] <- capacities[,2020,]
  
  shares <- capacities %>%
    as.quitte() %>%
    rename(PGALL = variable) %>%
    left_join(distinct(PGALLtoEF, PGALL, .keep_all = TRUE), by = "PGALL") %>%
    group_by(region, period, EF) %>%
    mutate(
      share = ifelse(PGALL %in% disaggregatetechs, value / sum(value, na.rm = TRUE), 1),
      share = ifelse(is.na(share) & PGALL %in% c("PGSOL","PGLHYD","PGAWND"), 1, share),
      share = ifelse(is.na(share), 0, share)
    ) %>%
    ungroup() %>%
    select(c("region", "period", "PGALL", "share"))
  
  return(shares)
}
