#' calcISubsiPerDemTech
#'
#' Use data to derive OPENPROM input parameter iSubsiPerDemTech
#'
#' @return  OPENPROM input data iSubsiPerDemTech
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "ISubsiPerDemTech", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select left_join full_join right_join case_when
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_rows

calcISubsiPerDemTech <- function() {
  
  fStartHorizon <- readEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fStartHorizon"]
  
  fEndHorizon  <- readEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fEndHorizon"]
  
  DSBS <- toolGetMapping(
    name = "DSBS.csv",
    type = "blabla_export",
    where = "mrprom"
  ) 
  
  TECH <- toolGetMapping(
    name = "TECH.csv",
    type = "blabla_export",
    where = "mrprom"
  )
  
  countries <- as.data.frame(getISOlist())
  names(countries) <- "region"
  
  years <- as.data.frame(fStartHorizon : fEndHorizon)
  names(years) <- "period"
  
  names(DSBS) <- "variable"
  
  x <- expand.grid(
    variable = DSBS[["variable"]],
    TECH = TECH[["TECH"]],
    region = countries[["region"]],
    period = years[["period"]]
  )
  
  x[["value"]] <- 0
  
  x <- as.quitte(x)
  
  x[["unit"]] <- 1
  
  x <- x %>%
    mutate(
      value = case_when(
        tech == "TELC" & variable == "PC" ~ 0.4,
        tech == "TELC" & variable == "IS"  ~ 0.1,
        tech == "TELC" & variable == "CH" ~ 0.1,
        tech == "TELC" & variable == "HOU"  ~ 0.3,
        tech == "HTDAC" & variable == "DAC" ~ 0.025,
        tech == "H2DAC" & variable == "DAC"  ~ 0.025,
        tech == "LTDAC" & variable == "DAC" ~ 0.025,
        tech == "TEW" & variable == "DAC"  ~ 0.025,
        TRUE ~ value
      )
    )
  
  x <- as.quitte(x) %>% as.magpie()
  
  # GDP for weights
  GDP <- calcOutput(type = "iGDP", aggregate = FALSE)
  GDP <- collapseDim(GDP, dim = 3)
  
  list(
    x = x,
    weight = GDP,
    unit = "1",
    description = "data"
  )
}
