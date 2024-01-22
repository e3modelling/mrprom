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
#' @importFrom tidyr drop_na nesting
#'

calcIInvPlants <- function() {
  
  x <- readSource("GEM", convert = TRUE)
  x <- x[, c(3:6, 10)]
  period <- NULL
  x <- x %>% drop_na(period)
  
  # load current OPENPROM set configuration
  sets <- readSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "PGALL")
  sets <- unlist(strsplit(sets[, 1], ","))
  
  # use iInvPlants-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "iInvPlants-mapping.csv",
                        type = "sectoral",
                        where = "mappingfolder")
  
  ## filter mapping to keep only XXX sectors
  map <- filter(map, map[, "OPEN.PROM"] %in% sets)
  
  x <- left_join(x, map, by = "variable") %>%
    select(-c("variable"))
  
  names(x) <- sub("OPEN.PROM", "variable", names(x))
  value <- NULL
  x <- mutate(x, value = sum(value, na.rm = TRUE), .by = c("region", "period", "variable", "unit"))
  x <- distinct(x)
  
  x <- as.quitte(x)
  
  mis_tech <- sets[which(!(sets %in% map[, "OPEN.PROM"]))]
  
  model <- NULL
  scenario <- NULL
  unit <- NULL
  region <- NULL
  variable <- NULL
  z <- expand(x, nesting(model, scenario, region, unit, period), variable = mis_tech)
  
  z["value"] <- NA

  x <- rbind(x, z)
  
  x <- x %>% filter(period %in% c(2010:max(x["period"])))
  
  x <- x %>% mutate(value = ifelse(is.na(value), 0, value))
  
  return(list(x = x,
              weight = NULL,
              unit = "GW",
              class = "quitte",
              description = "Installed Capacities of power plants from the 
              Global Energy Monitor"))
  
}
