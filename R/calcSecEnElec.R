#' calcSecEnElec
#'
#' Use Primes and Navigate data for Secondary Energy Electricity
#' 
#' @return Secondary Energy Electricity
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "SecEnElec", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select full_join
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom utils tail


calcSecEnElec <- function() {
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  #Primes Secondary Energy Electricity data
  a <- readSource("PrimesSEElec")
  
  a <- a[getRegions(a)[getRegions(a) %in% as.character(getISOlist())], , ]
  
  a <-  as.quitte(a) %>%
    interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
  
  a <- as.quitte(a) %>% as.magpie()
  
  a <- a[,fStartHorizon:max(getYears(a, as.integer = TRUE)),]
  
  suppressWarnings({
    a <- toolCountryFill(a, fill = NA)
  })
  
  a <- a / 1000 #to TWh
  
  getItems(a,3.3) <- "TWh"
  
  #filter navigate data by variable
  x <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = TRUE)
  
  x <- x[,,"Secondary Energy|Electricity"][,,"REMIND-MAgPIE 3_2-4_6"]
  
  x <- as.quitte(x)
  # #take the mean value from the available models
  # x <- mutate(x, value = mean(value, na.rm = TRUE), .by = c("scenario", "region", "period", "variable", "unit"))
  # 
  x[["model"]] <- "(Missing)"
  
  x <- unique(x)
  
  x <- as.quitte(x) %>% as.magpie()
  
  x<- x * 277.7778 #EJ to TWh
  
  getItems(x,3.3) <- "TWh"
  
  x <-  as.quitte(x) %>%
    interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
  
  x <- as.quitte(x) %>% as.magpie()
  
  suppressWarnings({
    x <- toolCountryFill(x, fill = NA)
  })

  x_Navigate <- x[,fStartHorizon : 2100,]
  
  #remove model and scenario dimension
  a <- collapseDim(a,3.1)
  x_Navigate <- collapseDim(x_Navigate,3.1)
  
  a_primes <- as.quitte(a)
  b_navigate <- as.quitte(x_Navigate)
  
  qx <- left_join(a_primes, b_navigate, by = c("model", "scenario", "region", "variable", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  
  x <- as.quitte(qx) %>% as.magpie()
  
  # set NA to 0
  x[is.na(x)] <- 10^-6
  
  list(x = x,
       weight = NULL,
       unit = "TWh",
       description = "Primes and Navigate Secondary Energy Electricity")
  
}
