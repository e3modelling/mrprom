#' calcFuelConsumption
#'
#' Read in several excel files with data from ENERDATA.
#' The dataset contains several energy-related data types from ENERDATA for
#' various countries and years.
#'
#' The two available ENERDATA datasets are combined into one,
#' NA values are replaced with zeros and historical data are corrected.
#'
#' Finally, the combined ENERDATA datasets are filtered by consumption, and also
#' allow further processing of specific variables which are related to the
#' consumption.
#'
#' @param subtype string. By choosing a subtype you filter the ENERDATA dataset
#' (the part of data which contains only the data of consumption) by type, to
#' allow further processing of variables that has to do with the consumption.
#'
#' @return The "ENERDATA" data filtered by consumption, type of consumption
#' and also corrected by historical data.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "FuelConsumption", file = "transport.csv" , aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>%
#' @importFrom quitte as.quitte


calcFuelConsumption <- function(subtype = "transport") {

  x <- readSource("ENERDATA", "consumption", convert = TRUE)
  x <- as.quitte(x)
  x <- filter(x, x[["variable"]] %in% grep(subtype, levels(x[["variable"]]),
                                           value = TRUE, ignore.case = TRUE))

  x[["value"]] <- as.numeric(x[["value"]])
  x[["variable"]] <- as.factor(x[["variable"]])
  x[["period"]] <- as.factor(x[["period"]])

  x[["variable"]] <- gsub(" ", "_", x[["variable"]])
  x[["variable"]] <- gsub(",", "", x[["variable"]])

  x <-as.quitte(x)
  x <- as.magpie(x)
  u <- getItems(x, "unit")

  return(list(x = x,
              weight = NULL,
              unit = u,
              description = "Enerdata; Consumption"))

}
