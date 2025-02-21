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


  x <- read.csv(file = paste0(subtype, ".csv"), header = FALSE)

  if (subtype == "total-vehicles-in-use") {
    x[1, 135] <- iconv(x[1, 135], from = "UTF-8", to = "ASCII//TRANSLIT")
    }

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
  x <- x[c(2:nrow(x)), ]
  names(x)[1] <- "period"
  x <- gather(x, "region", "value",  grep("[A-Z][A-Z][A-Z]", names(x), value = TRUE))
  x[["variable"]] <- subtype
  if (subtype %in% c("passenger-cars-in-use",
                     "buses-and-motor-coaches-in-use",
                     "total-vans,-pickups,-lorries-and-road-tractors-in-use")) {
    x[["unit"]] <- "vehicles"
  } else if (subtype %in% c("bus-and-motor-coach-traffic",
                            "lorry-and-road-tractor-traffic")) {
    x[["unit"]] <- "km/yr"
  } else if (subtype %in% c("inland-surface-freight-transport-by-inland-waterway",
                            "inland-surface-freight-transport-by-rail",
                            "inland-surface-freight-transport-by-road")) {
    x[["unit"]] <- "million tKm/yr"
  } else if (subtype %in% c("inland-surface-passenger-transport-by-rail",
                            "inland-surface-public-passenger-transport-by-road",
                            "inland-surface-passenger-transport-total",
                            "inland-surface-private-passenger-transport-by-road")) {
    x[["unit"]] <- "million pKm/yr"
  } else if (subtype %in% ("passenger-car-traffic")) {
    x[["unit"]] <- "million motor vehicles Km/yr"
  }  else if (subtype %in% c("total-van,-pickup,-lorry-and-road-tractor-traffic")) {
    x[["unit"]] <- "million motor vehicles Km/yr"
  }  else if (subtype %in% c("total-four-wheeled-traffic")) {
    x[["unit"]] <- "million motor vehicles Km/yr"
  }  else if (subtype %in% ("passenger-car-first-registrations")) {
    x[["unit"]] <- "vehicles"
  }

  x[["value"]] <- as.numeric(x[["value"]])
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
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
