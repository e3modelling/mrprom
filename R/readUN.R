#' readUN
#'
#'Read in a csv file and convert it to a magpie object
#'Reads the UN Open Data Energy Balances
#'https://data.un.org/SdmxBrowser/start
#'It contains various variables about balances, consumption, production, losses,
#'imports, exports and other.
#' 
#' @return The read-in data into a magpie object
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("UN")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom utils read.csv
#' @importFrom readr parse_number
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate

readUN <- function() {
  files <- list.files(".")

  x <- NULL
  for (i in files) {
    x1 <- read.csv(i, sep = ";")
    x <- rbind(x1,x)
  }
  
  map <- toolGetMapping(name = "UN_code_countries.csv",
                        type = "sectoral",
                        where = "mrprom")
  
  
  names(map) <- sub("UN_code", "REF_AREA", names(map))
  
  x <- left_join(x, map, by = "REF_AREA")
  
  names(x) <- sub("Country", "region", names(x))
  
  x[["region"]] <- toolCountry2isocode(x[["region"]], mapping = 
                                         c("C te d Ivoire" = "CIV",
                                           "China, Hong Kong Special Administrative Region" = "HKG",
                                           "China, Macao Special Administrative Region" = "MAC",
                                           "France-Monaco" = "FRA",
                                           "Italy-San Marino" = "ITA",
                                           "Norway-Svalbard and Jan Mayen Islands"= "NOR",
                                           "State of Palestine"= "PSE",
                                           "Switzerland-Liechtenstein"= "CHE",
                                           "Cura ao"= "CUW",
                                           "Saint Barth lemy"= "BLM"))
  
  map2 <- toolGetMapping(name = "UN_fuels.csv",
                        type = "sectoral",
                        where = "mrprom")
  
  
  names(map2) <- sub("UN_code", "COMMODITY", names(map2))
  map2 <- select(map2,c("COMMODITY","OPEN_PROM"))
  
  x <- left_join(x, map2, by = "COMMODITY")
  x <- select(x,c("TRANSACTION", "TIME_PERIOD","OBS_VALUE","region","OPEN_PROM"))
  
  names(x) <- sub("TIME_PERIOD", "period", names(x))
  names(x) <- sub("TRANSACTION", "variable", names(x))
  names(x) <- sub("OBS_VALUE", "value", names(x))
  names(x) <- sub("OPEN_PROM", "new", names(x))
  
  map3 <- toolGetMapping(name = "UN_variables.csv",
                         type = "sectoral",
                         where = "mrprom")
  
  names(map3) <- sub("UN_code", "variable", names(map3))
  
  map3 <- select(map3,c("OPEN_PROM","variable"))
  
  x <- left_join(x, map3, by = "variable")
  x <- select(x,c("period","value","region","new","OPEN_PROM"))
  names(x) <- sub("OPEN_PROM", "variable", names(x))
  
  x <- as.quitte(x)
  
  x[["unit"]] <- "Mtoe"
  
  x <- mutate(x, value = sum(value, na.rm = TRUE), .by = c("model","scenario","region","variable","unit","period"))
  
  x <- distinct(x)
  
  x <- x %>% drop_na()
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  #Terajoules to Mtoe
  x <- x / 41868000
  
  list(
    x = x,
    weight = NULL,
    description = c(
      category = "Energy Balances",
      type = "Energy Balances",
      filename = "UNSD+DF_UNData_EnergyBalance+1.0_2025_06_23_08_30_15.csv",
      `Indicative size (MB)` = 46,
      dimensions = "4D",
      unit = "Mtoe",
      Confidential = "E3M"
    )
  )
}
