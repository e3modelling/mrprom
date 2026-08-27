#' readREMIND_MAgPIE_PRISMA
#'
#' Read REMIND_MAgPIE_PRISMA emissions.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Christos Koumparakis
#'
#' @examples
#' \dontrun{
#' a <- readSource("REMIND_MAgPIE_PRISMA")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte interpolate_missing_periods
#'
readREMIND_MAgPIE_PRISMA <- function() {

  x <- read.csv("REMIND-MAgPIE 3.6-4.14_PRISMA.csv")
  colnames(x) <- sub("^X", "", colnames(x))
  x <- x %>% pivot_longer(!c("model","scenario","region","variable","unit" ), names_to = "period", values_to = "value")

  x[["region"]] <- toolCountry2isocode((x[["region"]]), mapping =
                                         c("REMIND-MAgPIE 3.6-4.14|Canada, Australia, New Zealand" = "CAZ",
                                           "REMIND-MAgPIE 3.6-4.14|China and Taiwan"               = "CHA",
                                           "REMIND-MAgPIE 3.6-4.14|EU 28"                          = "EU28",
                                           "REMIND-MAgPIE 3.6-4.14|India"                          = "IND",
                                           "REMIND-MAgPIE 3.6-4.14|Japan"                          = "JPN",
                                           "REMIND-MAgPIE 3.6-4.14|Latin America and the Caribbean"= "LAM",
                                           "REMIND-MAgPIE 3.6-4.14|Middle East and North Africa"   = "MEA",
                                           "REMIND-MAgPIE 3.6-4.14|Non-EU28 Europe"                = "NEU",
                                           "REMIND-MAgPIE 3.6-4.14|Other Asia"                     = "OAS",
                                           "REMIND-MAgPIE 3.6-4.14|Russia and Reforming Economies" = "REF",
                                           "REMIND-MAgPIE 3.6-4.14|Sub-Saharan Africa"             = "SSA",
                                           "REMIND-MAgPIE 3.6-4.14|United States of America"       = "USA",
                                           "World"                                                 = "GLO"))

  x <- filter(x, !is.na(x[["region"]]))
  x <- as.quitte(x)
  x <- interpolate_missing_periods(x, period = 2005:2100, expand.values = TRUE)
  x <- as.magpie(x)

  list(x = x,
       weight = NULL,
       description = c(category = "Greenhouse Gas Emissions",
                       type = "REMIND_MAgPIE_PRISMA Greenhouse Gas Emissions",
                       filename = "REMIND-MAgPIE 3.6-4.14_PRISMA.csv",
                       `Indicative size (MB)` = 8,
                       dimensions = "3D",
                       unit = "MtCO2",
                       Confidential = "E3M"))
}
