#' calcIDataElecProd
#'
#' Use ENERDATA electricity production data to derive OPENPROM input parameter iDataElecProd
#'
#' @return  OPENPROM input data iDataElecProd
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataElecProd", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom tidyr pivot_wider replace_na
#' @importFrom quitte as.quitte

calcIDataElecProd <- function() {
  share_of_solar <- calcOutput(type = "IInstCapPast", aggregate = FALSE)
  x <- readSource("ENERDATA", "production", convert = TRUE)

  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]

  years <- getYears(x, as.integer = TRUE)
  x <- x[, c(max(fStartHorizon, min(years)):max(years)), ]
  # load current OPENPROM set configuration
  sets <- toolGetMapping(
    name = "PGALL.csv",
    type = "blabla_export",
    where = "mrprom"
  )[, 1]

  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(
    name = "prom-enerdata-elecprod-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  )

  ## filter mapping to keep only XXX sectors
  map <- filter(map, map[, "PGALL"] %in% sets)
  ## ..and only items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  map <- map[map[, "ENERDATA"] %in% enernames, ]
  ## filter data to keep only XXX data
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])


  z <- enernames == "Electricity production from natural gas.GWh - Electricity production from cogeneration with natural gas.GWh"
  enernames[z] <- "Electricity production from natural gas.GWh"
  k <- enernames == "Electricity production from coal, lignite.GWh - Electricity production from coal.GWh"
  enernames[k] <- "Electricity production from coal, lignite.GWh"

  x <- x[, , enernames]

  b <- x[, , "Electricity production from cogeneration with natural gas.GWh"]
  c <- x[, , "Electricity production from coal.GWh"]

  x[, , "Electricity production from natural gas.GWh"] <- x[, , "Electricity production from natural gas.GWh"] - ifelse(is.na(b), 0, b)
  x[, , "Electricity production from coal, lignite.GWh"] <- x[, , "Electricity production from coal, lignite.GWh"] - ifelse(is.na(c), 0, c)
    
  l <- getNames(x) == "Electricity production from natural gas.GWh"
  getNames(x)[l] <- "Electricity production from natural gas.GWh - Electricity production from cogeneration with natural gas.GWh"
  v <- getNames(x) == "Electricity production from coal, lignite.GWh"
  getNames(x)[v] <- "Electricity production from coal, lignite.GWh - Electricity production from coal.GWh"

  ## rename variables to openprom names
  getNames(x) <- map[1:12,2]

  # share of PV, CSP
  share_of_PV <- share_of_solar[, , "PGSOL"] / (share_of_solar[, , "PGASOL"] + share_of_solar[, , "PGSOL"])
  share_of_CSP <- share_of_solar[, , "PGASOL"] / (share_of_solar[, , "PGASOL"] + share_of_solar[, , "PGSOL"])
  x_CSP <- x[, , "PGSOL"] * ifelse(is.na(share_of_CSP), mean(share_of_CSP, na.rm = TRUE), share_of_CSP)
  x[, , "PGSOL"] <- x[, , "PGSOL"] * ifelse(is.na(share_of_PV), mean(share_of_PV, na.rm = TRUE), share_of_PV)
  x_CSP <- collapseDim(x_CSP, 3.2)
  getItems(x_CSP, 3.1) <- "PGASOL"
  x <- mbind(x, x_CSP)

  # IEA Hydro Plants, replace NA
  b <- readSource("IEA", subtype = "ELOUTPUT") %>%
    as.quitte()

  qb <- b %>%
    filter(product == "HYDRO") %>%
    select(c("region", "period", "value"))

  qx <- as.quitte(x) %>%
    left_join(qb, by = c("region", "period")) %>%
    mutate(
      value.x = ifelse(variable == "PGLHYD" & is.na(value.x), value.y, value.x)
    ) %>%
    select(-value.y) %>%
    rename(value = value.x)

  # IEA gas turbine, replace NA
  qn <- b %>%
    filter(product == "NATGAS") %>%
    select(c("region", "period", "value")) %>%
    replace_na(list("value" = 0)) %>%
    distinct()

  qx <- qx %>%
    left_join(qn, by = c("region", "period")) %>%
    mutate(
      value.x = ifelse(variable == "ATHNGS" & is.na(value.x), value.y, value.x)
    ) %>%
    select(-c("value.y")) %>%
    rename(value = value.x)

  # IEA LIGNITE, replace NA
  ql <- b %>%
    filter(product == "LIGNITE") %>%
    replace_na(list("value" = 0)) %>%
    select(c("region", "period", "value")) %>%
    distinct()

  qx <- qx %>%
    left_join(ql, by = c("region", "period")) %>%
    mutate(
      value.x = ifelse(variable == "ATHLGN" & is.na(value.x), value.y, value.x)
    ) %>%
    select(-c("value.y")) %>%
    rename(value = value.x)

  # complete incomplete time series
  qx <- as.quitte(qx) %>%
    interpolate_missing_periods(period = getYears(x, as.integer = TRUE), expand.values = TRUE)

  z <- qx %>%
    replace_na(list("value" = 0)) %>%
    select(c("region", "variable", "unit", "period", "value")) %>%
    as.quitte() %>%
    as.magpie()

  list(
    x = collapseNames(z),
    weight = NULL,
    unit = getItems(z, 3.2)[1],
    description = "Enerdata; Electricity production"
  )
}
