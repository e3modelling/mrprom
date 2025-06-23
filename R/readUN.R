#' readUN
#'
#'Read in a csv file and convert it to a magpie object
#'Reads the UN Open Data
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
  
  
  list(
    x = x,
    weight = NULL,
    description = c(
      category = "Final energy consumption",
      type = "Final energy consumption",
      filename = "UNSD+DF_UNData_EnergyBalance+1.0_2025_06_23_08_30_15.csv",
      `Indicative size (MB)` = 46,
      dimensions = "4D",
      unit = "Mtoe",
      Confidential = "E3M"
    )
  )
}
