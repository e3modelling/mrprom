#' fullOPEN-PROM
#'
#' Read in several files with data from GEME3, IRF and WDI_PA and convert it
#' to a csv file.
#' The dataset contains several data types about transport, traffic, air
#' transport passengers per country and per year and Production Level and
#' Unit Cost data.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @importFrom dplyr %>% select left_join mutate
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte
#' @importFrom utils write.table
#'
#' @examples
#' \dontrun{
#'  a <- retrieveData("OPEN_PROM", regionmapping = "regionmappingOPDEV2.csv")
#' }

fullOPEN_PROM <- function() {

  calcOutput(type = "ACTV", file = "iACTV.csvr", aggregate = TRUE)

  for (i in c("NENSE", "DOMSE", "INDSE", "TRANSE")) {
    x <- calcOutput(type = "IFuelCons", subtype = i, aggregate = TRUE)
    x[is.na(x)] <- 0
    xq <- as.quitte(x) %>%
          select(c("period", "value", "region", "variable", "new")) %>% # nolint
          pivot_wider(names_from = "period") # nolint
    fheader <- paste("dummy,dummy,dummy", paste(colnames(xq)[4 : length(colnames(xq))], collapse = ","), sep = ",")
    writeLines(fheader, con = paste0("iFuelCons", i, ".csv"))
    write.table(xq,
                quote = FALSE,
                row.names = FALSE,
                file = paste0("iFuelCons", i, ".csv"),
                sep = ",",
                col.names = FALSE,
                append = TRUE)
  }

  x <- calcOutput("IFuelPrice", aggregate = FALSE)
  # compute weights for price aggregation
  map <- toolGetMapping(getConfig("regionmapping"), "regional")
  qx <- as.quitte(x)
  names(qx) <- sub("region", "ISO3.Code", names(qx))
  ## add mapping to dataset
  qx <- left_join(qx, map, by = "ISO3.Code")
  ## weight value is 1 / (number of non NA values for each year, country, variable, fuel)
  value <- NULL
  qx <- mutate(qx, value = 1 / length(which(!is.na(value))), .by = c("Region.Code", "period", "new", "variable"))
  names(qx) <- sub("ISO3.Code", "region", names(qx))
  qx <- select(qx, -c("model", "scenario", "Full.Country.Name", "Region.Code"))
  weight <- as.magpie(as.quitte(qx))
  # perform price aggregation
  x <- toolAggregate(x, weight = weight, rel = map, from = "ISO3.Code", to = "Region.Code")
  # write input data file that GAMS can read
  xq <- as.quitte(x) %>%
        select(c("period", "value", "region", "variable", "new")) %>% # nolint
        pivot_wider(names_from = "period") # nolint
  fheader <- paste("dummy,dummy,dummy", paste(colnames(xq)[4 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iFuelPrice.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iFuelPrice.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)



  return(list(x = x,
              weight = NULL,
              unit = "various",
              description = "OPENPROM input data"))

}

