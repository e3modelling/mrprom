#' calcIInstCapPast
#'
#' Use data from ENERDATA to derive OPENPROM input parameter iInstCapPast
#' This dataset contains the values of installed capacity for past years in GW.
#'
#' @return  OPENPROM input data iInstCapPast
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IInstCapPast", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join case_when if_else arrange
#' @importFrom tidyr pivot_wider spread gather
#' @importFrom quitte as.quitte
#' @importFrom tibble add_row

calcIInstCapPast <- function() {

  x <- readSource("ENERDATA", "capacity", convert = TRUE)
  avail <- calcOutput(type = "IAvailRate", aggregate = FALSE)

  #add it to PGADPV
  add_DPV <- x[,,"Solar installed electricity capacity from PV"]
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  x <- x[, c(max(fStartHorizon, min(getYears(x, as.integer = TRUE))) : max(getYears(x, as.integer = TRUE))), ]
  add_DPV <- add_DPV[, c(max(fStartHorizon, min(getYears(x, as.integer = TRUE))) : max(getYears(x, as.integer = TRUE))), ]
  
  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-enerdata-pgall-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")
  ## ..and only items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA..MW."]), "ENERDATA..MW."])
  map <- map[map[, "ENERDATA..MW."] %in% enernames, ]

  enernames <- unique(map[!is.na(map[, "ENERDATA..MW."]), "ENERDATA..MW."])
  enernames <- enernames[! enernames %in% c("")]

  z <- enernames == "Total electricity capacity coal, lignite (multifuel included) - Single fired electricity capacity lignite"
  enernames[z] <- "Total electricity capacity coal, lignite (multifuel included)"
  k <- enernames == "Total electricity capacity gas (multifuel oil/gas included) - Installed capacity in combined cycles"
  enernames[k] <- "Total electricity capacity gas (multifuel oil/gas included)"
  p <- enernames == "(Share of supercritical, ultrasupercritical and IGCC technologies in coal installed capacity.%)*(Total electricity capacity coal, lignite (multifuel included) - Single fired electricity capacity lignite)"
  enernames[p] <- "Share of supercritical, ultrasupercritical and IGCC technologies in coal installed capacity.%"
  n <- enernames == "Solar installed electricity capacity from CSP + Solar installed electricity capacity from PV"
  enernames[n] <- "Solar installed electricity capacity from CSP"
  
  x <- x[, , enernames]
  x <- mbind(x, add_DPV)

  b <- x[, , "Single fired electricity capacity lignite"]
  c <- x[, , "Installed capacity in combined cycles"]
  d <- x[, , "Share of supercritical, ultrasupercritical and IGCC technologies in coal installed capacity.%"] / 100
  
  x[, , "Total electricity capacity coal, lignite (multifuel included)"] <- x[, , "Total electricity capacity coal, lignite (multifuel included)"] - ifelse(is.na(b), 0, b)
  x[, , "Total electricity capacity gas (multifuel oil/gas included)"] <- x[, , "Total electricity capacity gas (multifuel oil/gas included)"] - ifelse(is.na(c), 0, c)
  x[, , "Solar installed electricity capacity from CSP"] <- x[, , "Solar installed electricity capacity from CSP"] + ifelse(is.na(add_DPV), 0, add_DPV)
  x[, , "Share of supercritical, ultrasupercritical and IGCC technologies in coal installed capacity.%"] <- x[, , "Share of supercritical, ultrasupercritical and IGCC technologies in coal installed capacity.%"] * x[, , "Total electricity capacity coal, lignite (multifuel included)"] / 100
  
  # remove from coal the Supercritical coal
  x[, , "Total electricity capacity coal, lignite (multifuel included)"] <- x[, , "Total electricity capacity coal, lignite (multifuel included)"] * (1 - ifelse(is.na(d), 0, d))
  
  l <- getNames(x) == "Total electricity capacity coal, lignite (multifuel included).MW"
  getNames(x)[l] <- "Total electricity capacity coal, lignite (multifuel included).MW - Single fired electricity capacity lignite.MW"
  v <- getNames(x) == "Total electricity capacity gas (multifuel oil/gas included).MW"
  getNames(x)[v] <- "Total electricity capacity gas (multifuel oil/gas included).MW - Installed capacity in combined cycles.MW"
  m <- getNames(x) == "Share of supercritical, ultrasupercritical and IGCC technologies in coal installed capacity.%"
  getNames(x)[m] <- "(Share of supercritical, ultrasupercritical and IGCC technologies in coal installed capacity.%)*(Total electricity capacity coal, lignite (multifuel included) - Single fired electricity capacity lignite)"
  r <- getNames(x) == "Solar installed electricity capacity from CSP.MW"
  getNames(x)[r] <- "Solar installed electricity capacity from CSP.MW + Solar installed electricity capacity from PV.MW"
  
  x <- x[,, getNames(x)[1:13]] #drop "Solar installed electricity capacity from PV"
  
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

  # Multiplying the capacity values by the availability rate
  avail_rates <- as.quitte(avail["GLO", "y2020",])[c("variable", "value")]

  qx <- qx %>%
    left_join(avail_rates, by = "variable") %>%
    mutate(value = value.x * value.y) %>%
    select(-c("value.x", "value.y"))

  # Converting MW values to GW
  qx[["value"]] <- qx[["value"]] / 1000

  # Converting to magpie object
  x <- as.quitte(qx) %>% as.magpie()
  # set NA to 0
  x[is.na(x)] <- 0
  list(x = x,
       weight = NULL,
       unit = "GW",
       description = "Enerdata; Installed capacity")
}
