#' readiTEM
#'
#'Read in a csv file and convert it to a magpie object
#'Reads the International Transport Energy Modeling (iTEM) Open Data
#'https://zenodo.org/records/4287423#.ZCr0YOxBzX0
#'It contains various variables about transport, population and emissions
#' 
#' @return The read-in data into a magpie object
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- readSource("iTEM")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom utils read.csv
#' @importFrom readr parse_number
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate

readiTEM <- function() {
  drop_columns <- c("Source", "Country", "Region")
  data <- read.csv("iTEM harmonized_dataset.csv")
  data <- data[, !names(data) %in% drop_columns]
               
  x <- data %>%
    pivot_longer(
    cols = starts_with("X"),
    names_to = "period",
    values_to = "value",
    values_drop_na = TRUE
    ) %>%
    mutate(
      period = parse_number(period)
    ) %>%
    rename(
      Region = ISO.Code,
      Vehicle_type = Vehicle.Type
    ) %>%
    as.quitte() %>%
    as.magpie()

  
  list(x = x,
       weight = NULL,
       description = c(category = "Transport",
                       type = "Transport, population and emissions",
                       filename = "iTEM harmonized_dataset.csv",
                       `Indicative size (MB)` = 1.5,
                       dimensions = "8D",
                       unit = "various",
                       Confidential = "open"))
}
