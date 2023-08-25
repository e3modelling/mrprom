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
#' @importFrom dplyr %>% select
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte
#'
#' @examples
#' \dontrun{
#'  a <- retrieveData("OPEN_PROM", regionmapping = "regionmappingOPDEV2.csv")
#' }

fullOPEN_PROM <- function() {

  calcOutput(type = "ACTV", file = "iACTV.csvr", aggregate = TRUE)
  for (i in c("NENSE", "DOMSE", "INDSE")) {
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


  return(list(x = x,
              weight = NULL,
              unit = "various",
              description = "OPENPROM input data"))

}

