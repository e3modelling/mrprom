#' calcIInstCapPast
#'
#' Creates the OPEN-PROM input parameter {iInstCapPast}, which represents
#' historical installed electricity generation capacity by technology and region.
#' ENERDATA provides installed capacity data for a subset of countries and regions.
#' To ensure complete geographical coverage across the 249-country structure used
#' by OPEN-PROM, the available data are mapped to the model's technology
#' classification using the {prom-enerdata-pgall-mapping.csv} correspondence
#' table and expanded to all countries and regions. Countries without available
#' source data are assigned zero values, ensuring a complete dataset for the full
#' model domain.
#' The function supports four alternative calculation modes. In the {Total}
#' mode, installed capacities are obtained directly from ENERDATA and mapped to
#' OPEN-PROM power generation technologies. In the {TotalEff} mode, the same
#' ENERDATA capacities are combined with technology-specific availability rates
#' obtained from {IAvailRate}. Installed capacities are multiplied by the
#' corresponding availability factors and converted from megawatts (MW) to
#' gigawatts (GW), yielding effective available capacity.
#' In the {NonCHP} and {CHP} modes, installed capacities are estimated
#' from historical electricity generation data rather than directly from ENERDATA.
#' Electricity production values from {IDataElecProd} are divided by the
#' number of hours in a year (8,760 hours) to derive equivalent generation
#' capacities expressed in GW. These modes provide separate estimates for
#' non-combined heat and power (NonCHP) and combined heat and power (CHP)
#' technologies.
#' Missing values are replaced with zero, and all technology mappings and
#' adjustments are applied consistently across regions and years. The resulting
#' dataset provides historical installed generation capacity by technology and
#' serves as an input dataset for the OPEN-PROM modeling framework.
#'
#' @return  OPENPROM input data iInstCapPast
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios, Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IInstCapPast", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join case_when if_else arrange
#' @importFrom tidyr pivot_wider spread gather replace_na
#' @importFrom quitte as.quitte
#' @importFrom tibble add_row

calcIInstCapPast <- function(mode = "TotalEff") {
  if (mode == "Total") {
    capacities <- getCap()
  } else if (mode == "TotalEff") {
    x <- getCap()
    # Multiplying the capacity values by the availability rate
    avail <- calcOutput(type = "IAvailRate", aggregate = FALSE)
    avail_rates <- as.quitte(avail) %>% select(c("region", "period", "variable", "value"))
    years <- getYears(x, as.integer = TRUE)

    capacities <- as.quitte(x) %>%
      interpolate_missing_periods(period = years, expand.values = TRUE) %>%
      left_join(avail_rates, by = c("region", "period", "variable")) %>%
      # Applying avail rates & converting MW values to GW
      mutate(value = value.x * value.y / 1000) %>%
      select(c("region", "variable", "period", "value")) %>%
      as.quitte() %>%
      as.magpie()
  } else if (mode %in% c("NonCHP", "CHP")) {
    hoursYear <- 8760
    capacities <- calcOutput(
      type = "IDataElecProd", mode = mode, aggregate = FALSE
    ) / hoursYear
  }

  list(
    x = capacities,
    weight = NULL,
    unit = "GW",
    description = "Enerdata; Installed capacity"
  )
}

# Helper --------------------------------------------------------------
getShares <- function(data) {
  shares <- chp %>%
    as.quitte() %>%
    left_join(map, by = "product", relationship = "many-to-many") %>%
    rename(variable = "variable.y") %>%
    select(c("region", "period", "value", "variable")) %>%
    group_by(region, period, variable) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    drop_na() %>%
    left_join(techProd, by = c("region", "period", "variable")) %>%
    mutate(value = value.x / value.y) %>%
    replace_na(list(value = 0)) %>%
    select("region", "period", "variable", "value")
}

getCap <- function() {
  x <- readSource("ENERDATA", "capacity", convert = TRUE)
  # filter years
  fStartHorizon <- toolReadEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  years <- getYears(x, as.integer = TRUE)
  x <- x[, c(max(fStartHorizon, min(years)):max(years)), ]

  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(
    name = "prom-enerdata-pgall-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  )
  ## ..and only items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA..MW."]), "ENERDATA..MW."])
  map <- map[map[, "ENERDATA..MW."] %in% enernames, ]

  enernames <- unique(map[!is.na(map[, "ENERDATA..MW."]), "ENERDATA..MW."])
  enernames <- enernames[!enernames %in% c("")]

  x <- x[, , enernames]
  ## rename variables from ENERDATA to openprom names
  getItems(x, 3.1) <- map[!(map[, 2] == ""), 1]
  x[is.na(x)] <- 0
  x[, , "ATHCOAL"] <- x[, , "ATHCOAL"] - x[, , "ATHLGN"]
  return(x)
}
