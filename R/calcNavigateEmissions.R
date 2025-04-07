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
  x <- as.magpie(x)
  
  # set NA to 0
  x[is.na(x)] <- 10^-6
  
  list(x = x,
       weight = NULL,
       unit = "Mtoe",
       description = "Navigate Emissions")
  
}
