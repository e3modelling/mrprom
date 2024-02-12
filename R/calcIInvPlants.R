#' calcIInvPlants
#'
#' Use installed capacities of power plants data from the Global Energy Monitor,
#' to derive OPENPROM input parameter iInvPlants.
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
  sets <- readSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "PGALL")
  sets <- unlist(strsplit(sets[, 1], ","))

  # use iInvPlants-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "iInvPlants-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")

  ## filter mapping to keep only XXX sectors
  map <- filter(map, map[, "OPEN.PROM"] %in% sets)

  x <- left_join(x, map, by = "variable") %>%
    select(-c("variable"))
  
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
  #if value is NA assign it to 1e-08
  x <- x %>% mutate(value = ifelse(is.na(value), 1e-08, value)) %>%
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
