#' readEuropeanHydrogen
#'
#' Read EuropeanHydrogen demand from European Hydrogen Observatory.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("EuropeanHydrogen", convert = TRUE)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readEuropeanHydrogen <- function() {
  
  x1 <- read_excel("Hydrogen demand 2022.xlsx")
  
  x2 <- read_excel("Hydrogen demand 2023.xlsx")
  
  x1 <- x1[,c(1,2,6)]
  x2 <- x2[,c(1,2,6,7)]
  
  x1[["period"]] <- 2022
  names(x1) <- c("region", "variable", "value", "period")
  names(x2) <- names(x1)
  
  x <- rbind(x1, x2)
  
  x <- as.quitte(x)
  
  x[["unit"]] <- "Mtoe"
  #fix units
  x[["value"]] <- x[["value"]] * 10^-6 / 2.87
  
  suppressWarnings({
    x[["region"]] <- toolCountry2isocode((x[["region"]]), mapping =
                                           c("Luxemburg" = "LUX"))
  })

  x <- as.quitte(x)
  x <- filter(x, !is.na(x[["region"]]))
  
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "EuropeanHydrogen demand from European Hydrogen Observatory",
                       type = "Hydrogen demand",
                       filename = "Hydrogen demand 2023.xlsx",
                       `Indicative size (MB)` = 0.5,
                       dimensions = "3D",
                       unit = "Mtoe",
                       Confidential = "E3M"))
}
