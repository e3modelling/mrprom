#' calcTREMOVE
#'
#' Use TREMOVE data for fuel consumption in TRANSE sector
#'
#' @return TREMOVE fuel consumption in TRANSE sector
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "TREMOVE", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select full_join
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom utils tail


calcTREMOVE <- function() {
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  # load current OPENPROM set configuration
  sets <- toolGetMapping(paste0("TRANSE.csv"),
                         type = "blabla_export",
                         where = "mrprom")
  
  sets <- as.character(sets[, 1])
  
  #TREMOVE data
  a <- readSource("TREMOVE", subtype = "FuelOutlook")
  
  a <- a[getRegions(a)[getRegions(a) %in% as.character(getISOlist())], , ]
  
  a <- a / 1000 #ktoe to mtoe
  
  #add Passenger Light Duty Vehicles to PC
  PC <- a[,,"Passenger Light Duty Vehicles"]
  
  getItems(a,3.3) <- "Mtoe"
  getItems(a,3.5) <- "Final Energy Demand"
  
  map_TREMOVE <- toolGetMapping(name = "prom-TREMOVE-fucon-mapping.csv",
                                type = "sectoral",
                                where = "mrprom")
  
  #remove the empty cells from mapping
  map_TREMOVE <- map_TREMOVE[!(map_TREMOVE[, "FUEL"] == ""), ]
  
  map_TREMOVE <- filter(map_TREMOVE, map_TREMOVE[, "SBS"] %in% sets)
  
  a <- toolAggregate(a[, , as.character(unique(map_TREMOVE[["FUEL"]]))], dim = 3.2, rel = map_TREMOVE, from = "FUEL", to = "EF")
  a <- toolAggregate(a[, , as.character(unique(map_TREMOVE[["TREMOVE"]]))], dim = 3.4, rel = map_TREMOVE, from = "TREMOVE", to = "SBS")
  
  PC <- toolAggregate(PC[, , as.character(unique(map_TREMOVE[["FUEL"]]))], dim = 3.2, rel = map_TREMOVE, from = "FUEL", to = "EF")
  getItems(PC,3.4) <- "PC"
  
  a[,,"PC"] <- a[,,"PC"] + ifelse(is.na(PC), mean(PC, na.rm=TRUE), PC)
  
  a <- a[,,"REF"]
  
  a <- as.quitte(a)
  
  names(a) <- sub("variable", "new", names(a))
  names(a) <- sub("technology", "variable", names(a))
  
  a <- select(a, -("sector"))
  
  a[,"scenario"] <- "(Missing)"
  
  a <-  as.quitte(a) %>%
    interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
 
  x <- as.quitte(a) %>% as.magpie()
  
  suppressMessages(
    suppressWarnings(
      x <- toolCountryFill(x, fill = NA)
    )
  )
  
  # set NA to 0
  x[is.na(x)] <- 10^-6
  x <- x[,fStartHorizon : 2100,]
  
  list(x = x,
       weight = NULL,
       unit = "Mtoe",
       description = "TREMOVE fuel consumption in TRANSE sector")
  
}
