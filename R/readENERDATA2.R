#' readENERDATA2
#'
#' Read a csv files with data from ENERDATA.
#' The dataset contains several energy-related data types from ENERDATA for
#' various countries and years.
#'
#'
#' @return The read-in data into a magpie object
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("ENERDATA2")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom utils read.csv
#' @importFrom tidyr pivot_longer
#'
#'


readENERDATA2 <- function() {

  x <- read.csv("open_prom_database_20_4_2023_1.csv")
  x[["Countries"]] <- factor(x[["Countries"]])
  x <- filter(x, x[["Sheet_Name"]] != "CO2_FF")
  x <- filter(x, !is.na("Countries"))
  x <- filter(x, !is.na("Unit"))
  x[["Enerdata_Title"]] <- factor(x[["Enerdata_Title"]])
  levels(x[["Enerdata_Title"]]) <- sub("^ ", "", levels(x[["Enerdata_Title"]]))
  levels(x$Enerdata_Title)<-sub("Installed electricity capacity of co-generation gas Installed electricity capacity of co-generation gas","Installed electricity capacity of co-generation gas", levels(x$Enerdata_Title)) #nolint
  x <- pivot_longer(x[, c(1, 2, 23:54, 60)], cols = c(3:34)) #nolint
  names(x) <- c("region", "unit", "variable", "period", "value")
  x[, "value"] <- as.numeric(unlist(x[, "value"]))
  x <- x %>% distinct()
  x <- filter(x, !is.na("value")) #nolint
  x <- filter(x, !x[["variable"]]%in%c(" ", "")) #nolint
  x$period <- sub("X", "", x$period)
  x$period <- as.numeric(x$period)
  x[["variable"]] <- factor(x[["variable"]])

  x <- filter(x, !is.na("region")) #nolint

  x$variable <- factor(x$variable)
  levels(x$variable) <- sub("\\.", "", levels(x$variable))

  x <- as.quitte(x)

  return(as.magpie(x))
}
