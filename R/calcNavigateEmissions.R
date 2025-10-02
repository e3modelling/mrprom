#' calcNavigateEmissions
#'
#' Use Navigate Emissions to reporting.
#'
#' @return Navigate Emissions as magpie object
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "NavigateEmissions", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select full_join
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom utils tail
#' 
#' @export

calcNavigateEmissions <- function() {
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  # use navigate-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "NavigateEmissions.csv",
                        type = "sectoral",
                        where = "mrprom")
  
  #filter navigate data by variable
  x1 <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = TRUE)
  x2 <- readSource("Navigate", subtype = "SUP_1p5C_Default", convert = TRUE)
  x3 <- readSource("Navigate", subtype = "SUP_2C_Default", convert = TRUE)
  
  x1 <- x1[,,map[,"Emissions"]]
  
  x2 <- x2[,,map[,"Emissions"]]
  
  x3 <- x3[,,map[,"Emissions"]]
  
    
  x <- mbind(x1, x2, x3)
  
  x <- x[,,"REMIND-MAgPIE 3_2-4_6"]
  
  x <- as.quitte(x)
  x <- interpolate_missing_periods(x, 2010:2100, expand.values = TRUE)
  
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  
  qx <- select(x, -c("model", "scenario"))
  qx_bu <- qx
  
  ## assign to countries with NA, their H12 region with weights calculated from population
  
  population <- calcOutput(type = "POP", aggregate = FALSE)
  population <- as.quitte(population)
  
  # compute weights by population
  names(population) <- sub("region", "CountryCode", names(population))
  
  ## add mapping to population
  population <- left_join(population, h12, by = "CountryCode")
  value.x <- NULL
  value.y <- NULL
  weights <- NULL
  value <- NULL
  
  POP <- mutate(population, weights = sum(value, na.rm = TRUE), .by = c("period"))
  POP["weights"] <- POP["value"] / POP["weights"]
  names(POP) <- sub("CountryCode", "region", names(POP))
  POP <- select(POP, -c("value", "model", "scenario", "X", "RegionCode", "variable", "unit"))
  qx <- left_join(qx, POP, by = c("region", "period"))
  
  qx <- mutate(qx, value = sum(value, na.rm = TRUE), .by = c("period", "variable", "unit"))
  
  qx["value"] <- qx["value"] * qx["weights"]
  
  qx <- select(qx, -c("weights"))
  
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  x <- as.quitte(qx) %>% as.magpie()
  
  # set NA to 0
  x[is.na(x)] <- 10^-6
  
  list(x = x,
       weight = NULL,
       unit = "Mtoe",
       description = "Navigate Emissions")
  
}
