#' readREMIND_MAgPIE_SoCDR
#'
#' Read REMIND_MAgPIE_SoCDR emissions.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("REMIND_MAgPIE_SoCDR")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readREMIND_MAgPIE_SoCDR <- function() {
  
  x <- read.csv("REMIND-MAgPIE 3.5-4.11_SoCDR.csv")
  colnames(x) <- sub("^X", "", colnames(x))
  x <- x %>% pivot_longer(!c("model","scenario","region","variable","unit" ), names_to = "period", values_to = "value")
  
  x[["region"]] <- toolCountry2isocode((x[["region"]]), mapping =
                                         c("REMIND-MAgPIE 3.5-4.11|Canada, Australia, New Zealand" = "CAZ",
                                           "REMIND-MAgPIE 3.5-4.11|China and Taiwan"               = "CHA",
                                           "REMIND-MAgPIE 3.5-4.11|EU 28"                          = "EU28",
                                           "REMIND-MAgPIE 3.5-4.11|India"                          = "IND",
                                           "REMIND-MAgPIE 3.5-4.11|Japan"                          = "JPN",
                                           "REMIND-MAgPIE 3.5-4.11|Latin America and the Caribbean"= "LAM",
                                           "REMIND-MAgPIE 3.5-4.11|Middle East and North Africa"   = "MEA",
                                           "REMIND-MAgPIE 3.5-4.11|Non-EU28 Europe"                = "NEU",
                                           "REMIND-MAgPIE 3.5-4.11|Other Asia"                     = "OAS",
                                           "REMIND-MAgPIE 3.5-4.11|Russia and Reforming Economies" = "REF",
                                           "REMIND-MAgPIE 3.5-4.11|Sub-Saharan Africa"             = "SSA",
                                           "REMIND-MAgPIE 3.5-4.11|United States of America"       = "USA",
                                           "World"                                                 = "GLO"))
  
  x <- filter(x, !is.na(x[["region"]]))
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "Greenhouse Gas Emissions",
                       type = "REMIND_MAgPIE_SoCDR Greenhouse Gas Emissions",
                       filename = "REMIND-MAgPIE 3.5-4.11_SoCDR.csv",
                       `Indicative size (MB)` = 8,
                       dimensions = "3D",
                       unit = "MtCO2",
                       Confidential = "E3M"))
}
