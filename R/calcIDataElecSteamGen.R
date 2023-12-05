#' calcIDataElecSteamGen
#'
#' Use data to derive OPENPROM input parameter iDataElecSteamGen
#'
#' @return  OPENPROM input data iDataElecSteamGen
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataElecSteamGen", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join case_when if_else arrange
#' @importFrom tidyr pivot_wider spread gather
#' @importFrom quitte as.quitte

calcIDataElecSteamGen <- function() {

  x <- readSource("ENERDATA", "capacity", convert = TRUE)
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  x <- x[, c(max(fStartHorizon, min(getYears(x, as.integer = TRUE))) : max(getYears(x, as.integer = TRUE))), ]
  
  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-enerdata-pgall-mapping.csv",
                        type = "sectoral",
                        where = "mappingfolder")
  ## ..and only items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA..MW."]), "ENERDATA..MW."])
  map <- map[map[, "ENERDATA..MW."] %in% enernames, ]

  enernames <- unique(map[!is.na(map[, "ENERDATA..MW."]), "ENERDATA..MW."])
  enernames <- enernames[! enernames %in% c("")]

  z <- enernames == "Total electricity capacity coal, lignite (multifuel included) - Single fired electricity capacity lignite"
  enernames[z] <- "Total electricity capacity coal, lignite (multifuel included)"
  k <- enernames == "Total electricity capacity gas (multifuel oil/gas included) - Installed capacity in combined cycles"
  enernames[k] <- "Total electricity capacity gas (multifuel oil/gas included)"

  x <- x[, , enernames]
  x[, , "Total electricity capacity coal, lignite (multifuel included)"] <- x[, , "Total electricity capacity coal, lignite (multifuel included)"] - x[, , "Single fired electricity capacity lignite"]
  x[, , "Total electricity capacity gas (multifuel oil/gas included)"] <- x[, , "Total electricity capacity gas (multifuel oil/gas included)"] - x[, , "Installed capacity in combined cycles"]
  
  l <- getNames(x) == "Total electricity capacity coal, lignite (multifuel included).MW"
  getNames(x)[l] <- "Total electricity capacity coal, lignite (multifuel included).MW - Single fired electricity capacity lignite.MW"
  v <- getNames(x) == "Total electricity capacity gas (multifuel oil/gas included).MW"
  getNames(x)[v] <- "Total electricity capacity gas (multifuel oil/gas included).MW - Installed capacity in combined cycles.MW"

  ## rename variables from ENERDATA to openprom names
  ff <- map[!(map[, 2] == ""), 1]
  getNames(x) <- ff

  qx <- as.quitte(x) %>%
    interpolate_missing_periods(period = getYears(x, as.integer = TRUE), expand.values = TRUE)
  qx_bu <- qx
  # assign to countries with NA, their H12 region mean
  h12 <- toolGetMapping("regionmappingH12.csv", where = "madrat")
  names(qx) <- sub("region", "CountryCode", names(qx))
  ## add h12 mapping to dataset
  qx <- left_join(qx, h12, by = "CountryCode")
  ## add new column containing regional mean value
  value <- NULL
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("RegionCode", "period", "variable"))
  names(qx) <- sub("CountryCode", "region", names(qx))
  qx <- select(qx, -c("model", "scenario", "X", "RegionCode"))
  qx_bu <- select(qx_bu, -c("model", "scenario"))
  ## assign to countries with NA, their H12 region mean
  value.x <- NULL
  value.y <- NULL
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))
  ## assign to countries that still have NA, the global mean
  qx_bu <- qx
  qx <- mutate(qx, value = mean(value, na.rm = TRUE), .by = c("period", "variable"))
  qx <- left_join(qx_bu, qx, by = c("region", "variable", "period", "unit")) %>%
    mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
    select(-c("value.x", "value.y"))

  # Multiplying each power plant tech by availability rate (from PRIMES)
  variable <- NULL
  TOTCAP <- NULL
  PEAKLOAD <- NULL
  region <- NULL
  period <- NULL
  unit <- NULL

  qx <- qx %>%
    mutate(value = case_when(
    variable == "ATHLGN" ~ value * 0.85,
    variable == "ATHHCL" ~ value * 0.85,
    variable == "ATHRFO" ~ value * 0.80,
    variable == "ATHNGS" ~ value * 0.80,
    variable == "ATHBMSWAS" ~ value * 0.85,
    variable == "ACCGT" ~ value * 0.80,
    variable == "PGNUC" ~ value * 0.90,
    variable == "PGLHYD" ~ value * 0.67,
    variable == "PGWND" ~ value * 0.23,
    variable == "PGSOL" ~ value * 0.20,
    variable == "PGASOL" ~ value * 0.25,
    variable == "PGAWNO" ~ value * 0.32,
    variable == "PGAOTHREN" ~ value * 0.45,
    TRUE ~ value))

  # Calculating TOTCAP as a sum of all capacities
  qx <- mutate(qx, value = sum(value, na.rm = TRUE), .by = c("region", "period"))
  qx["variable"] <- "TOTCAP"
  qx <- unique(qx)
  # Convert TOTCAP unit to GW
  qx <- qx %>%
    mutate(value = if_else(variable == "TOTCAP", value / 1000, value))

  # Adding PEAKLOAD and other variables
  qx <- qx %>%
    spread(key = variable, value = value) %>%
    mutate(
    PEAKLOAD = TOTCAP * 0.9,
    BASELOAD = PEAKLOAD * 0.3576,
    Non_CHP_Per = 0.00000001,
    CHP_Cap = 0.00000001,
    CHP_ELC = 0.00000001,
  # STE1CL = 0.0, STE1CH = 0.0, STE1CD = 0.0,
  # STE1CR = 0.0, STE1CG = 0.0, STE1CB = 0.0,
    STE1AL = 0.0, STE1AH = 0.0, STE1AD = 0.0,
    STE1AR = 0.0, STE1AG = 0.0, STE1AB = 0.0,
    STE1AH2F = 0.0) %>%
    gather(key = "variable", value = "value", -region, -period, -unit) %>%
    arrange(region, period, variable)

  # Converting to magpie object
  x <- as.quitte(qx) %>% as.magpie()
  # set NA to 0
  x[is.na(x)] <- 0
  list(x = x,
       weight = NULL,
       unit = "GW",
       description = "Enerdata; Installed capacity")
}
