#' readIRF
#'
#' Read in an csv file and convert it to a magpie object.
#' The dataset contains several data types about transport and traffic.
#' The data is from https://datawarehouse.worldroadstatistics.org/users/login
#' The International Road Federation serves a network of public and private
#' sector members by providing world-class knowledge resources,
#' advocacy services, and continuing education programs which together offer a
#' global marketplace for best practices and industry solutions"
#'
#' @param subtype By choosing a subtype you can select the corresponding csv
#' file and convert it to a magpie object.
#' Available types are:
#' \itemize{
#' \item `passenger-cars-in-use`:
#' \item `buses-and-motor-coaches-in-use`:
#' \item `total-vans,-pickups,-lorries-and-road-tractors-in-use`:
#' \item `bus-and-motor-coach-traffic`:
#' \item `lorry-and-road-tractor-traffic`:
#' \item `inland-surface-freight-transport-by-inland-waterway`:
#' \item `inland-surface-freight-transport-by-rail`:
#' \item `inland-surface-freight-transport-by-road`:
#' \item `inland-surface-passenger-transport-by-rail`:
#' \item `inland-surface-public-passenger-transport-by-road`:
#' \item `passenger-car-traffic`:
#' \item `total-van,-pickup,-lorry-and-road-tractor-traffic`:
#' \item `passenger-car-first-registrations`:
#' \item `total-vehicles-in-use`:
#' }
#'
#' @return The read-in data into a magpie object
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IRF")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom utils read.csv
#' @importFrom tidyr gather
#'
readIRF <- function(subtype = "passenger-cars-in-use") {

  if (subtype == "total-vehicles-in-use") {
    x1 <- read.csv(file = paste0("total-vehicles-in-use", ".csv"), header = FALSE)
    x2 <- read.csv(file = paste0("total-four-wheeled-vehicles-in-use-(n)", ".csv"), header = FALSE)
    x1[1, 135] <- iconv(x1[1, 135], from = "UTF-8", to = "ASCII//TRANSLIT")
    x1 <- combineCSV(x1, subtype)
    x2 <- combineCSV(x2, subtype)
    x <- full_join(x2, x1, by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    x[["unit"]] <- "vehicles"
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "passenger-cars-in-use") {
    x1 <- read.csv(file = paste0("passenger-cars-in-use", ".csv"), header = FALSE)
    x2 <- read.csv(file = paste0("passenger-cars-in-use-(n)", ".csv"), header = FALSE)
    x1 <- combineCSV(x1, subtype)
    x2 <- combineCSV(x2, subtype)
    x <- full_join(x2, x1, by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    x[["unit"]] <- "vehicles"
    x <- as.quitte(x)
    x <- as.magpie(x)
  }

  if (subtype == "buses-and-motor-coaches-in-use") {
    x1 <- read.csv(file = paste0("buses-and-motor-coaches-in-use", ".csv"), header = FALSE)
    x2 <- read.csv(file = paste0("buses-and-motor-coaches-in-use-(n)", ".csv"), header = FALSE)
    x1 <- combineCSV(x1, subtype)
    x2 <- combineCSV(x2, subtype)
    x <- full_join(x2, x1, by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    x[["unit"]] <- "vehicles"
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "total-vans,-pickups,-lorries-and-road-tractors-in-use") {
    x1 <- read.csv(file = paste0("total-vans,-pickups,-lorries-and-road-tractors-in-use", ".csv"), header = FALSE)
    x2 <- read.csv(file = paste0("total-vans,-pickups,-lorries-and-road-tractors-in-use-(n)", ".csv"), header = FALSE)
    x1 <- combineCSV(x1, subtype)
    x2 <- combineCSV(x2, subtype)
    x <- full_join(x2, x1, by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    x[["unit"]] <- "vehicles"
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "bus-and-motor-coach-traffic") {
    x1 <- read.csv(file = paste0("bus-and-motor-coach-traffic", ".csv"), header = FALSE)
    x2 <- read.csv(file = paste0("bus-and-motor-coach-traffic-(mio-vehicle-km-annual)", ".csv"), header = FALSE)
    x1 <- combineCSV(x1, subtype)
    x2 <- combineCSV(x2, subtype)
    x <- full_join(x2, x1, by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    x[["unit"]] <- "motor vehicles km per year"
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "lorry-and-road-tractor-traffic") {
    x1 <- read.csv(file = paste0("lorry-and-road-tractor-traffic", ".csv"), header = FALSE)
    x2 <- read.csv(file = paste0("lorry-and-road-tractor-traffic-(mio-vehicle-km-annual)", ".csv"), header = FALSE)
    x1 <- combineCSV(x1, subtype)
    x2 <- combineCSV(x2, subtype)
    x <- full_join(x2, x1, by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    x[["unit"]] <- "motor vehicles km per year"
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "inland-surface-freight-transport-by-inland-waterway") {
    x1 <- read.csv(file = paste0("inland-surface-freight-transport-by-inland-waterway", ".csv"), header = FALSE)
    x2 <- read.csv(file = paste0("inland-surface-freight-transport-by-inland-waterway-(mio-tonne-km)", ".csv"), header = FALSE)
    x1 <- combineCSV(x1, subtype)
    x2 <- combineCSV(x2, subtype)
    x <- full_join(x2, x1, by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    x[["unit"]] <- "million tonne km per year"
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "inland-surface-freight-transport-by-rail") {
    x1 <- read.csv(file = paste0("inland-surface-freight-transport-by-rail", ".csv"), header = FALSE)
    x2 <- read.csv(file = paste0("inland-surface-freight-transport-by-rail-(mio-tonne-km)", ".csv"), header = FALSE)
    x1 <- combineCSV(x1, subtype)
    x2 <- combineCSV(x2, subtype)
    x <- full_join(x2, x1, by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    x[["unit"]] <- "million tonne km per year"
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "inland-surface-freight-transport-by-road") {
    x1 <- read.csv(file = paste0("inland-surface-freight-transport-by-road", ".csv"), header = FALSE)
    x2 <- read.csv(file = paste0("inland-surface-freight-transport-by-road-(mio-tonne-km)", ".csv"), header = FALSE)
    x1 <- combineCSV(x1, subtype)
    x2 <- combineCSV(x2, subtype)
    x <- full_join(x2, x1, by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    x[["unit"]] <- "million tonne km per year"
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "inland-surface-passenger-transport-by-rail") {
    x1 <- read.csv(file = paste0("inland-surface-passenger-transport-by-rail", ".csv"), header = FALSE)
    x2 <- read.csv(file = paste0("inland-surface-passenger-transport-by-rail-(mio-passenger-km)", ".csv"), header = FALSE)
    x1 <- combineCSV(x1, subtype)
    x2 <- combineCSV(x2, subtype)
    x <- full_join(x2, x1, by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    x[["unit"]] <- "million passenger km per year"
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "inland-surface-public-passenger-transport-by-road") {
    x1 <- read.csv(file = paste0("inland-surface-public-passenger-transport-by-road", ".csv"), header = FALSE)
    x2 <- read.csv(file = paste0("inland-surface-public-passenger-transport-by-road-(mio-passenger-km)", ".csv"), header = FALSE)
    x1 <- combineCSV(x1, subtype)
    x2 <- combineCSV(x2, subtype)
    x <- full_join(x2, x1, by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    x[["unit"]] <- "million passenger km per year"
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "inland-surface-passenger-transport-total") {
    x1 <- read.csv(file = paste0("inland-surface-passenger-transport-total", ".csv"), header = FALSE)
    x2 <- read.csv(file = paste0("inland-surface-passenger-transport---total-(mio-passenger-km)", ".csv"), header = FALSE)
    x1 <- combineCSV(x1, subtype)
    x2 <- combineCSV(x2, subtype)
    x <- full_join(x2, x1, by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    x[["unit"]] <- "million passenger km per year"
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "inland-surface-private-passenger-transport-by-road") {
    x1 <- read.csv(file = paste0("inland-surface-private-passenger-transport-by-road", ".csv"), header = FALSE)
    x2 <- read.csv(file = paste0("inland-surface-private-passenger-transport-by-road-(mio-passenger-km)", ".csv"), header = FALSE)
    x1 <- combineCSV(x1, subtype)
    x2 <- combineCSV(x2, subtype)
    x <- full_join(x2, x1, by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    x[["unit"]] <- "million passenger km per year"
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "passenger-car-traffic") {
    x1 <- read.csv(file = paste0("passenger-car-traffic", ".csv"), header = FALSE)
    x2 <- read.csv(file = paste0("passenger-car-traffic-(mio-vehicle-km-annual)", ".csv"), header = FALSE)
    x1 <- combineCSV(x1, subtype)
    x2 <- combineCSV(x2, subtype)
    x <- full_join(x2, x1, by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    x[["unit"]] <- "motor vehicle km per year"
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "total-van,-pickup,-lorry-and-road-tractor-traffic") {
    x1 <- read.csv(file = paste0("total-van,-pickup,-lorry-and-road-tractor-traffic", ".csv"), header = FALSE)
    x2 <- read.csv(file = paste0("total-van,-pickup,-lorry-and-road-tractor-traffic-(mio-vehicle-km-annual)", ".csv"), header = FALSE)
    x1 <- combineCSV(x1, subtype)
    x2 <- combineCSV(x2, subtype)
    x <- full_join(x2, x1, by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    x[["unit"]] <- "million motor vehicle km per year"
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "total-four-wheeled-traffic") {
    x1 <- read.csv(file = paste0("total-four-wheeled-traffic", ".csv"), header = FALSE)
    x2 <- read.csv(file = paste0("total-four-wheeled-traffic-(mio-vehicle-km-annual)", ".csv"), header = FALSE)
    x1 <- combineCSV(x1, subtype)
    x2 <- combineCSV(x2, subtype)
    x <- full_join(x2, x1, by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    x[["unit"]] <- "million motor vehicle km per year"
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  
  if (subtype == "passenger-car-first-registrations") {
    x1 <- read.csv(file = paste0("passenger-car-first-registrations", ".csv"), header = FALSE)
    x2 <- read.csv(file = paste0("passenger-car-first-registrations-(n)", ".csv"), header = FALSE)
    x1 <- combineCSV(x1, subtype)
    x2 <- combineCSV(x2, subtype)
    x <- full_join(x2, x1, by = c("model", "scenario", "region", "period", "variable", "unit")) %>%
      mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
      select(-c("value.x", "value.y"))
    x[["unit"]] <- "number of cars"
    x <- as.quitte(x)
    x <- as.magpie(x)
  }

  
  list(x = x,
       weight = NULL,
       description = c(category = "Transport",
                       type = "Transport and Traffic",
                       filename = "passenger-car-traffic.csv",
                       `Indicative size (MB)` = 0.068,
                       dimensions = "2D",
                       unit = "varius",
                       Confidential = "open"))

}

# Helpers --------------------------------------------------------------
combineCSV <- function(x, subtype) {
  
  suppressWarnings({
    names(x) <- toolCountry2isocode(as.character(x[1, ]), mapping = c("China, Hong Kong" = "HKG",
                                                                      "China, Macao" = "MAC",
                                                                      "Egypt, Arab Rep." = "EGY",
                                                                      "Korea, Rep." = "KOR",
                                                                      "Micronesia, Fed. Sts." = "FSM",
                                                                      "St. Helena" = "SHN",
                                                                      "St. Kitts and Nevis" = "KNA",
                                                                      "St. Lucia" = "LCA",
                                                                      "St. Vincent and the Grenadines" = "VCT",
                                                                      "Venezuela, RB" = "VEN",
                                                                      "Yemen, Rep." = "YEM",
                                                                      "Congo, Dem. Rep." = "COD",
                                                                      "Iran, Islamic Rep." = "IRN",
                                                                      "Gambia, The" = "GMB",
                                                                      "Sao Tome and Principe" = "STP",
                                                                      "Category" = "NA"))
  })
  
  x <- x[c(2:nrow(x)), ]
  names(x)[1] <- "period"
  x <- gather(x, "region", "value",  grep("[A-Z][A-Z][A-Z]", names(x), value = TRUE))
  x[["variable"]] <- subtype
  x[["value"]] <- as.numeric(x[["value"]])
  
  x <- as.quitte(x)
  return(x)
  
}
