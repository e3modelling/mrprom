#' calcIDecomPlants
#'
#' Use decommisioning capacities of power plants data from the Global Energy Monitor,
#' to derive OPENPROM input parameter iIDecomPlants.
#'
#' @return  OPENPROM input data iIDecomPlants.
#' The output data calculated from the "Global Energy Monitor".
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDecomPlants", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>% mutate filter select distinct left_join
#' @importFrom tidyr drop_na nesting expand complete
#'

calcIDecomPlants <- function() {

  #the dataset x from readGEM is quitte object(decommisioned and installed plants in GW)
  x <- readSource("GEM", convert = TRUE)
  #keep data that has to do with GW decided to be decommisioned
  x <- x[, c(3:5, 7, 10)]
  names(x) <- sub("retired year", "period", names(x))
  period <- NULL
  x <- x %>% drop_na(period)

  # load current OPENPROM set configuration
  
  sets <- toolGetMapping(name = "PGALL.csv",
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
  region <- NULL
  unit <- NULL
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
  x <- interpolate_missing_periods(x, period = 2010:2070, expand.values = TRUE)

  # Convert to magpie object
  x <- as.magpie(x)

  # Only keeping the ISO countries
  x <- x[getISOlist(), , ]
  
  #drop 2071
  x <- x[,2010:2070,]


  return(list(x = x,
              weight = NULL,
              unit = "GW",
              class = "magpie",
              description = "Decommisioning Capacities of power plants from the 
              Global Energy Monitor"))

}
