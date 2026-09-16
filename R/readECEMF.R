#' readECEMF
#'
#' Read ECEMF
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("ECEMF")
#' }
#'
#' @importFrom dplyr select mutate across
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel
#' @importFrom quitte as.quitte
#' @importFrom readxl read_excel
#'
readECEMF <- function() {
  
  x <- read_excel("percentage_water_space_heating.xlsx", col_names = TRUE, skip = 1)
  
  x <- x %>%
    pivot_longer(
      cols = -`...1`,
      names_to = "period",
      values_to = "value"
    ) %>%
    rename(region = `...1`) %>%
    mutate(period = as.integer(period))

  x[["variable"]] <- "i02ShareWSpHeat"
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "ECEMF",
                       type = "ECEMF",
                       filename = "percentage_water_space_heating.xlsx",
                       `Indicative size (MB)` = 0.18,
                       dimensions = "3D",
                       unit = "%",
                       Confidential = "E3M"))
}
