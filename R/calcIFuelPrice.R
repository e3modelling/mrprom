#' calcIFuelPrice
#'
#' Use ENERDATA fuel price data to derive OPENPROM input parameter iFuelPrice
#'
#' @return  OPENPROM input data iFuelPrice
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IFuelPrice", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>%
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte


calcIFuelPrice <- function() {

  # load data source (ENERDATA)
  x <- readSource("ENERDATA", "constant price", convert = TRUE)

  # filter years
  years <- toolReadSetFromGDX(system.file(file.path("extdata", "blabla.gdx"), package = "mrprom"), "datay")
  x <- x[, c(min(as.numeric(years[, "a"])) : max(getYears(x, as.integer = TRUE))), ]

  xtmp <- NULL
  for (i in c( "DOMSE", "INDSE")) {
  # load current OPENPROM set configuration
  sets <- toolReadSetFromGDX(system.file(file.path("extdata", "fulldata.gdx"), package = "mrprom"), i)

  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-enerdata-fuprice-mapping.csv",
                        type = "sectoral",
                        where = "mappingfolder")

  ## filter mapping to keep only i sectors
  map <- filter(map, map[, "SBS"] %in% sets[, 1])
  ## ..and only items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  map <- map[map[, "ENERDATA"] %in% enernames, ]
  ## filter data to keep only i data
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  tmp <- x[, , enernames]
  ## rename variables to openprom names
  ### add a dummy dimension to data because mapping has 3 dimensions, and data ony 2
  tmp <- add_dimension(tmp, dim = 3.2)
  ### rename variables
  getNames(tmp) <- paste0(paste(map[, 2], map[, 3], sep = "."), ".", sub("^.*.\\..*.\\.", "", getNames(tmp)))
  tmp <- mbind(xtmp, tmp)
  }
  x <- tmp
  # set NA to 0
  x[is.na(x)] <- 0

  list(x = x,
       weight = NULL,
       unit = "various",
       description = "Enerdata; fuel consumption in XXX sector")

}
