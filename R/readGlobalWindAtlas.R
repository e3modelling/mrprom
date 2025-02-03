#' readGlobalWindAtlas
#'
#' Read in wind powerDensity csv files from GlobalWindAtlas and convert it to a
#' magpie object. The data has information about wind potentials.
#'
#' @return magpie object with the requested output data about
#' wind potentials.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("GlobalWindAtlas")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>%
#' @importFrom utils read.csv
#'

readGlobalWindAtlas <- function() {
  
  Files <- list.files(pattern = ".csv")
  CSVlist <- lapply(Files, read.csv)
  names(CSVlist) <- Files
  x <- bind_rows(CSVlist, .id = "region")
  
  x[["region"]] = substr(x[["region"]], 1, nchar(x[["region"]])-4)
  
  x[["region"]] <- toolCountry2isocode(x[["region"]])
  
  x[["val"]] <- x[["val"]] * 1e-9 #w to gw
  x[["val"]] <- x[["val"]] * 1000000 #sqKm to sqm
  x[["val"]] <- x[["val"]] / 100 #percent
  x[["val"]] <- x[["val"]] * x[["perc"]]
  
  names(x)[names(x) == "val"] <- "value"
  
  #take 4%
  x <- x[seq(from = 2, to = nrow(x), by = 50), ]
  x <- x[,c(1:2)]
  x[["period"]] <- 2025
  x[["unit"]] <- "GW"
  
  x <- as.quitte(x) %>% as.magpie()
  x <- toolCountryFill(x, fill = NA)
  
  a <- readSource("LandAreaCountries")
  #multiply by sqKm
  x <- x * a
  
  x <- as.quitte(x)
  
  x[["variable"]] <- "WindPotential"
  
  x <- x %>% select(-c("unit1"))
  
  x <- as.quitte(x) %>% as.magpie()
  
  list(x = x,
       weight = NULL,
       description = c(category = "Wind Potential",
                       type = "Wind Potential",
                       filename = "countries.csv",
                       `Indicative size (MB)` = 0.198,
                       dimensions = "2D",
                       unit = "GW",
                       Confidential = "open"))
  
}