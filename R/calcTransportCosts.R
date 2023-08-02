#' calcTransportCosts
#'
#' @return Read TechCosts data and convert it to a csv file
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "TransportCosts", file = "iTransportCosts.csv", aggregate = FALSE)
#' }
#' @importFrom quitte as.quitte
#' @importFrom dplyr group_by slice %>%
#' @importFrom tidyr drop_na
#'

calcTransportCosts <- function() {

  x1 <- readSource("TechCosts", subtype = "Medium_cars")
  x1 <- add_dimension(x1, dim = 3.6, add = "transport_mode", nm = "Medium_cars")
  x2 <- readSource("TechCosts", subtype = "Small_cars")
  x2 <- add_dimension(x2, dim = 3.6, add = "transport_mode", nm = "Small_cars")
  x3 <- readSource("TechCosts", subtype = "maritime")
  x3 <- add_dimension(x3, dim = 3.6, add = "transport_mode", nm = "maritime")
  x4 <- readSource("TechCosts", subtype = "Inland_navigation")
  x4 <- add_dimension(x4, dim = 3.6, add = "transport_mode", nm = "Inland_navigation")
  x5 <- readSource("TechCosts", subtype = "Rail")
  x5 <- add_dimension(x5, dim = 3.6, add = "transport_mode", nm = "Rail")
  x6 <- readSource("TechCosts", subtype = "Aviation")
  x6 <- add_dimension(x6, dim = 3.6, add = "transport_mode", nm = "Aviation")
  x7 <- readSource("TechCosts", subtype = "2wheelers")
  x7 <- add_dimension(x7, dim = 3.6, add = "transport_mode", nm = "2wheelers")
  x8 <- readSource("TechCosts", subtype = "Bus_coach")
  x8 <- add_dimension(x8, dim = 3.6, add = "transport_mode", nm = "Bus_coach")
  x9 <- readSource("TechCosts", subtype = "HGVs>16t")
  x9 <- add_dimension(x9, dim = 3.6, add = "transport_mode", nm = "HGVs>16t")
  x10 <- readSource("TechCosts", subtype = "HGVs<16t")
  x10 <- add_dimension(x10, dim = 3.6, add = "transport_mode", nm = "HGVs<16t")
  x11 <- readSource("TechCosts", subtype = "LCVs")
  x11 <- add_dimension(x11, dim = 3.6, add = "transport_mode", nm = "LCVs")
  x12 <- readSource("TechCosts", subtype = "Large_cars")
  x12 <- add_dimension(x12, dim = 3.6, add = "transport_mode", nm = "Large_cars")

  x <- mbind(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)
  x <- as.quitte(x)

  x[["period_variable"]] <- NULL
  x[["period_variable"]] <- paste0(x[["variable"]], x[["period"]])
  period_variable <- NULL
  value <- NULL
  x <- x %>%  group_by(period_variable) %>%  slice(which.min(value)) %>% drop_na()

  x <- x[, -c(5, 8, 9, 10, 12)]

  x[["value"]] <- as.numeric(x[["value"]])
  x[["variable"]] <- as.factor(x[["variable"]])
  x[["period"]] <- as.factor(x[["period"]])

  x <- as.quitte(x)
  x <- as.magpie(x)

  return(list(x = x,
              weight = NULL,
              unit = "1",
              description = "readTechCosts; TransportCosts"))
}
