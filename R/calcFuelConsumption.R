#' calcFuelConsumption
#'
#' The two available ENERDATA datasets are combined into one filtered by
#' consumption, and allow further processing of a specific type of consumption.
#'
#' @param subtype string. Specific type of consumption.
#'
#' @return The "ENERDATA" data (filtered by consumption and type of consumption).
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "FuelConsumption", file = "transport.csv" , aggregate = FALSE)
#' }
#'
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

  return(list(x = x,
              weight = NULL,
              unit = "1",
              description = "Enerdata; Consumption"))

}
