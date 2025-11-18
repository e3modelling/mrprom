#' calcIRateLossesFinCons
#'
#' Use IEA and IRF data to derive OPENPROM input parameter iRateLossesFinCons
#'
#' @return  OPENPROM input data iRateLossesFinCons
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IRateLossesFinCons", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIRateLossesFinCons <- function() {
  fEndHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndHorizon"]

  dl <- calcOutput("IDataDistrLosses", aggregate = FALSE)
  x <- calcOutput("IFuelCons2", aggregate = FALSE)
  years <- Reduce(intersect, list(getYears(x), getYears(dl)))
  
  #take items that start with STE
  items <- getItems(x, 3.2)
  ste_items <- grep("^STE", items, value = TRUE)
  STE_fuel_cons <- dimSums(x[,,ste_items],3)
  
  fuels <- intersect(getItems(x, 3.2), getItems(dl, 3.1))
  
  #consumption
  consumption <- dimSums(collapseNames(x[, , fuels]), 3.1, na.rm = TRUE)
  
  #take items that start with STE
  consumption <-add_columns(consumption, addnm = "STE", dim = 3, fill = NA)
  consumption[,,"STE"] <- STE_fuel_cons
  
  #DistrLosses / consumption
  dl <- collapseDim(dl, 3.2)
  dl <- dl[,,getItems(consumption,3)]
  x <- dl / consumption
  x[is.infinite(x)] <- 0
  x[is.na(x)] <- 0
  # Converting to quitte object and interpolating periods
  qx <- as.quitte(x) %>% interpolate_missing_periods(expand.values = TRUE, period = min(getYears(x, as.integer = TRUE)) : fEndHorizon)

  # Converting to magpie object
  x <- as.quitte(qx) %>% as.magpie()
  x[x > 0.5] <- 0.5
  
  list(x = x,
       weight = NULL,
       unit = "(1)",
       description = "Rate of Distribution Losses")

}
