#' calcIFinConsSubFuel
#'
#' Use ENERDATA fuel consumption data to derive OPENPROM input parameter iFinConsSubFuel
#' (fuel consumption in NENSE sectors, i.e. PCH,NEN,BU - non energy and bunkers)
#'
#'
#' @return  OPENPROM input data iFinConsSubFuel
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IFinConsSubFuel", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% select
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte


calcIFinConsSubFuel <- function() {

  # load data source (ENERDATA)
  x <- readSource("ENERDATA", "consumption", convert = TRUE)

  # load current OPENPROM set configuration
  sets <- toolReadSetFromGDX(system.file(file.path("extdata", "fulldata.gdx"), package = "mrprom"), "NENSE")

  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-enerdata-fucon-mapping.csv",
                        type = "sectoral",
                        where = "mappingfolder")

  ## filter mapping to keep only NENSE sectors
  map <- filter(map, map[, "SBS"] %in% sets[, 1])
  ## ..and only items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  map <- map[map[, "ENERDATA"] %in% enernames, ]
  ## filter data to keep only NENSE data
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  x <- x[, , enernames]
  ## rename variables to openprom names
  ### add a dummy dimension to data because mapping has 3 dimensions, and data ony 2
  x <- add_dimension(x, dim = 3.2)
  ## rename variables
  getNames(x) <- paste0(paste(map[, 2], map[, 3], sep = "."), ".", sub("^.*.\\..*.\\.", "", getNames(x)))

  xq <- as.quitte(x) %>%
        select(c("period", "value", "region", "variable", "new")) %>% # nolint
        pivot_wider(names_from = "period") # nolint

  fheader <- paste("dummy,dummy,dummy", paste(colnames(xq)[4 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "test4.csv")
  write.table(xq, quote = FALSE, row.names = FALSE, file = "test4.csv", sep = ",", col.names = FALSE, append = TRUE)


  list(x = x,
       weight = NULL,
       unit = "various",
       description = "Enerdata; fuel consumption in NENSE sectors, i.e. PCH,NEN,BU - non energy and bunkers")

}
