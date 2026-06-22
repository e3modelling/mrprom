#' calcIInvPlants
#'
#' The Global Energy Monitor (GEM) dataset provides information on installed and planned
#' power plant capacities for 202 countries. The readGEM() function imports these data,
#' retaining only entries corresponding to power plants that are planned or decided to be
#' installed. All remaining countries in the 249-country structure used by calcOutput are
#' assigned zero values to ensure complete country coverage.
#' The GEM technology categories are mapped to the OPEN-PROM power generation
#' technology set (PGALL) using the mapping file iInvPlants-mapping.csv. Technologies that
#' are not part of the current OPEN-PROM configuration are excluded, while technologies
#' included in OPEN-PROM but not represented in the GEM dataset are added with missing
#' values.
#' Capacity values are aggregated by country, year, technology, and unit by summing all
#' corresponding plant capacities. The dataset is then completed to include all country–
#' technology–year combinations. Missing values are replaced with zero, representing the
#' absence of planned capacity additions.
#' Annual values are generated through interpolation for the period 2010–2045. The final
#' dataset is converted into a MAgPIE object, restricted to ISO countries, and returned as the
#' OPEN-PROM input parameter iInvPlants. The resulting output provides planned and
#' announced power plant capacity additions in GW by country, year, and technology.
#'
#' @return  OPENPROM input data iInvPlants.
#' The output data calculated from the "Global Energy Monitor".
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IInvPlants", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>% mutate filter select  distinct left_join
#' @importFrom tidyr drop_na nesting expand complete
#'

calcIInvPlants <- function() {

  ##the dataset x from readGEM is quitte object(decommisioned and installed plants in GW)
  x <- readSource("GEM", convert = TRUE)
  #keep data that has to do with GW decided to be installed
  x <- x[, c(3:6, 10)]
  period <- NULL
  x <- x %>% drop_na(period)

  # load current OPENPROM set configuration
  sets <- toolGetMapping(paste0("PGALL.csv"),
                         type = "blabla_export",
                         where = "mrprom")
  
  sets <- as.character(sets[, 1])

  # use iInvPlants-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "iInvPlants-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")

  ## filter mapping to keep only XXX sectors
  map <- filter(map, map[, "OPEN.PROM"] %in% sets)

  x <- left_join(x, map, by = "variable") %>%
    select(-c("variable"))
  
  x <- x[!is.na(x[,"OPEN.PROM"]), ]
  
  #take the sum of each plant per year in each country
  names(x) <- sub("OPEN.PROM", "variable", names(x))
  value <- NULL
  x <- mutate(x, value = sum(value, na.rm = TRUE), .by = c("region", "period", "variable", "unit"))
  
  #drop the common data
  x <- distinct(x)

  x <- as.quitte(x)
  #add missing variables with value NA
  mis_tech <- sets[which(!(sets %in% map[, "OPEN.PROM"]))]

  model <- NULL
  scenario <- NULL
  unit <- NULL
  region <- NULL
  variable <- NULL
  z <- expand(x, nesting(model, scenario, region, unit, period), variable = mis_tech)

  z["value"] <- NA

  x <- rbind(x, z)
  #starting year is 2010
  x <- x %>% filter(period %in% c(2010:max(x["period"])))
  #complete the missing data with NA
  x <- complete(x, model, scenario, region, variable, unit, period)
  #if value is NA assign it to 0
  x <- x %>% mutate(value = ifelse(is.na(value), 0, value)) %>%
    as.quitte()

  # Interpolating missing periods
  x <- interpolate_missing_periods(x, period = 2010:2045, expand.values = TRUE)

  # Convert to magpie object
  x <- as.magpie(x)

  # Only keeping the ISO countries
  x <- x[getISOlist(), , ]

  return(list(x = x,
              weight = NULL,
              unit = "GW",
              class = "magpie",
              description = "Installed Capacities of power plants from the 
              Global Energy Monitor"))

}
