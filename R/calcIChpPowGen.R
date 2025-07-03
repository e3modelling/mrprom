#' calcIChpPowGen
#'
#' Use data from EU Reference Scenario to derive OPENPROM input table imDataChpPowGen
#' This dataset includes CHP economic and technical data initialisation for electricity production.
#' The availability factor is hardcoded and it is based on previous data. 
#' 
#' @return magpie object with OPENPROM input data iChpPowGen 
#' 
#' @author Alexandros
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IChpPowGen", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select filter rename mutate case_when
#' @importFrom tidyr pivot_wider spread gather
#' @importFrom quitte as.quitte interpolate_missing_periods
 
calcIChpPowGen <- function() {
  
  x <- readSource("TechCosts2024", "PowerAndHeat", convert = TRUE)
  xeff <- readSource("TechCosts2024", "PowerAndHeatEfficiency", convert = TRUE)

  # Rename variable names to have the same variable names as in OPEN-PROM
  getNames(x,dim=1)[1] <-"IC"
  getNames(x,dim=1)[2] <-"FC"
  getNames(x,dim=1)[3] <-"VOM"
  getNames(x,dim=1)[4] <-"LFT"  

  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fEndHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fEndHorizon"]

  # Get steam subset from GAMS code
  map <- toolGetMapping(name = "prom-primes-chp-mapping.csv",
                         type = "sectoral",
                         where = "mrprom")

  xq <- as.quitte(x)
  xeffq <- as.quitte(xeff)
  
  # Convert the technology info from the variable column and assign a real variable name
  xeffq_clean <- xeffq %>%
  mutate(
    technology = as.character(variable),    
    variable = "BOILEFF"                  
  )
  # Match Column Types With xq
  xeffq_clean <- xeffq_clean %>%
    mutate(across(c(model, scenario, region, variable, unit, technology), as.character))

  xq <- xq %>%
    mutate(across(c(model, scenario, region, variable, unit, technology), as.character))

  xqeff <- bind_rows(xq, xeffq_clean)

  merged <- merge(map, xqeff, by.x = "PRIMES", by.y = "technology") # INNER JOIN

  # Dropping columns
  xq <- select(merged, -c("PRIMES"))

  set_ste <- as.character(map[, 1])

  df <- data.frame(OPEN.PROM=set_ste,
                  variable = "AVAIL",
                   period = 2020,
                   region = "GLO",
                   unit = "Percentage",
                   value = 0.0)

  # FIXME: The plant availability rates are missing from EU Reference Scenario 2020
  df <- mutate(df, value = case_when(
  OPEN.PROM == "STE1AL" ~ 0.85, OPEN.PROM == "STE1AH" ~ 0.85,
  OPEN.PROM == "STE1AD" ~ 0.29,     OPEN.PROM == "STE1AR" ~ 0.8,  OPEN.PROM == "STE1AG" ~ 0.8, 
  OPEN.PROM == "STE1AB" ~ 0.85, OPEN.PROM == "STE1AH2F" ~ 0.8, TRUE ~ value))

  xq <- bind_rows(xq, df)

  # Interpolating the missing values for the specified time period
  xq <- as.quitte(xq)
  xq <- interpolate_missing_periods(xq, seq(fStartHorizon, fEndHorizon, 1), expand.values = TRUE)
  # Rename columns
  xq <- rename(xq, "technology" = open.prom)

  # Converting to magpie object
  x <- as.quitte(xq) %>% as.magpie()
  # Set NA to 0
  x[is.na(x)] <- 0
  list(x = collapseNames(x),
       weight = NULL,
       unit = getItems(x, 3.2)[1],
       description = "EU Reference Scenario 2020; Data for power generation (CHP) costs (various)")
}
