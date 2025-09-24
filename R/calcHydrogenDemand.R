#' calcHydrogenDemand
#'
#' Use HydrogenDemand data for consumption 
#'
#' @return HydrogenDemand fuel consumption
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "HydrogenDemand", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% select
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom utils tail


calcHydrogenDemand <- function() {
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  x <- readSource("EuropeanHydrogen", convert = TRUE)
  
  #current OPENPROM set configuration
  map <- data.frame(
    EuropeanHydrogen = c("Refining","Other chemicals","Industrial heat",                  
                         "Ammonia","Mobility","Other",                           
                         "Steel","Power generation","Methanol",                         
                         "E-fuels","Blending in natural gas pipelines","Residential heat"),
    variable = c("","CH","OI",
                 "CH","PC","",
                 "IS","","CH",
                 "","","HOU"),
    new = rep("H2F", 12))
  
  
  #remove the empty cells from mapping
  map <- map[!(map[, "variable"] == ""), ]
  
  x[is.na(x)] <- 10^-6
  x <- toolAggregate(x[, , as.character(unique(map[["EuropeanHydrogen"]]))], dim = 3.1, rel = map, from = "EuropeanHydrogen", to = c("variable"))
   
  x <- as.quitte(x)
  
  x[["new"]] <- "H2F"
  
  x <-  as.quitte(x) %>%
    interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
  
  x <- as.quitte(x) %>% as.magpie()
  
  x <- x[,fStartHorizon : 2100,]
  
  list(x = x,
       weight = NULL,
       unit = "Mtoe",
       description = "HydrogenDemand consumption")
  
}
