#' calcIFuelCons
#'
#' Use ENERDATA fuel consumption data to derive OPENPROM input parameter iFuelConsXXX
#' (XXX: NENSE, INDSE, DOMSE)
#'
#' @param subype string, OPENPROM sector (DOMSE, INDSE, NENSE)
#' @return  OPENPROM input data iFuelConsXXX
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IFuelCons", subtype = "DOMSE", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% select
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte


calcIFuelCons <- function(subtype = "DOMSE") {

  # load data source (ENERDATA)
  x <- readSource("ENERDATA", "consumption", convert = TRUE)

  # filter years
  years <- toolReadSetFromGDX(system.file(file.path("extdata", "blabla.gdx"), package = "mrprom"), "datay")
  x <- x[, as.numeric(years[, "a"]), ]

  # load current OPENPROM set configuration
  sets <- toolReadSetFromGDX(system.file(file.path("extdata", "fulldata.gdx"), package = "mrprom"), subtype)

  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-enerdata-fucon-mapping.csv",
                        type = "sectoral",
                        where = "mappingfolder")

  ## filter mapping to keep only XXX sectors
  map <- filter(map, map[, "SBS"] %in% sets[, 1])
  ## ..and only items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  map <- map[map[, "ENERDATA"] %in% enernames, ]
  ## filter data to keep only XXX data
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  x <- x[, , enernames]
  ## rename variables to openprom names
  ### add a dummy dimension to data because mapping has 3 dimensions, and data ony 2
  x <- add_dimension(x, dim = 3.2)
  ### rename variables
  getNames(x) <- paste0(paste(map[, 2], map[, 3], sep = "."), ".", sub("^.*.\\..*.\\.", "", getNames(x)))

  # set NA to 0
  x[is.na(x)] <- 0

  list(x = x,
       weight = NULL,
       unit = "various",
       description = "Enerdata; fuel consumption in XXX sector")

}
