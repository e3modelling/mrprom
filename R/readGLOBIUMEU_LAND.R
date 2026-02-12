#' readGLOBIOMEU_LAND
#'
#' Read Emissions|CO2|AFOLU|LAND of all EU countries from GLOBIOMEU
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("GLOBIOMEU_LAND")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter select distinct
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#' @importFrom readxl read_excel excel_sheets
#'
readGLOBIOMEU_LAND <- function() {
  
  sheets <- lapply(excel_sheets("ref2020_lulucf_emissions.xlsx"), read_excel, path = "ref2020_lulucf_emissions.xlsx")
  
  x <- NULL
  for (i in 2 : length(sheets)) {
    y <- as.data.frame(sheets[i])
    region <- names(y)[1]
    y <- y[c(1:16),]
    names(y) <- y[1,]
    y <- y[-1,-c(2:3)]
    y[["region"]] <- region
    names(y) <- gsub("LULUCF emissions and removals in Mt CO2-eq", "variable",names(y))
    y <- y %>% pivot_longer(!c("region","variable"), names_to = "period", values_to = "value")
    y <- as.quitte(y)
    y[["unit"]] <- "Mt CO2/yr"
    x <- rbind(x, y)
  }
  
  suppressWarnings({
    x[["region"]] <- toolCountry2isocode(x[["region"]],mapping = c("EU27" = "EU27",
                                                                   "Czech.Republic" = "CZE",
                                                                   "The.Netherlands" = "NLD"))
  })
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  x[is.na(x)] <- 0
  
  list(x = x,
       weight = NULL,
       description = c(category = "Greenhouse Gas Emissions",
                       type = "Emissions|CO2",
                       filename = "ref2020_lulucf_emissions.xlsx",
                       `Indicative size (MB)` = 0.137,
                       dimensions = "2D",
                       unit = "Mt CO2/yr",
                       Confidential = "project"))
}
