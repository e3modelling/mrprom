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
#' @importFrom dplyr filter %>% mutate select summarise
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
  getNames(x) <- map[1:12, 2]

  # share of PV, CSP
  share_of_PV <- share_of_solar[, , "PGSOL"] / (share_of_solar[, , "PGCSP"] + share_of_solar[, , "PGSOL"])
  share_of_CSP <- share_of_solar[, , "PGCSP"] / (share_of_solar[, , "PGCSP"] + share_of_solar[, , "PGSOL"])
  x_CSP <- x[, , "PGSOL"] * ifelse(is.na(share_of_CSP), mean(share_of_CSP, na.rm = TRUE), share_of_CSP)
  x[, , "PGSOL"] <- x[, , "PGSOL"] * ifelse(is.na(share_of_PV), mean(share_of_PV, na.rm = TRUE), share_of_PV)
  x_CSP <- collapseDim(x_CSP, 3.2)
  getItems(x_CSP, 3.1) <- "PGCSP"
  x <- mbind(x, x_CSP)
  years <- getYears(x, as.integer = TRUE)

  z <- readSource("IEA", subtype = "ELOUTPUT") %>%
    as.quitte() %>%
    imputeIEA(as.quitte(x)) %>% # Impute NA values based on IEA data
    interpolate_missing_periods(period = years, expand.values = TRUE) %>%
    replace_na(list("value" = 0)) %>%
    select(c("region", "variable", "unit", "period", "value")) %>%
    as.quitte() %>%
    as.magpie() %>%
    add_dimension(dim = 3.2, add = "unit", nm = "GWh")

  list(
    x = collapseNames(z),
    weight = NULL,
    unit = getItems(z, 3.2)[1],
    description = "Enerdata; Electricity production"
  )
}

# Helper ------------------------------------------------
imputeIEA <- function(iea_data, open_data) {
  iea_vars <- list(
    "ATHBMSWAS" = c(
      "INDWASTE", "MUNWASTER",
      "MUNWASTEN", "PRIMSBIO", "BIOGASES", "BIOGASOL",
      "BIODIESEL", "OBIOLIQ", "RENEWNS", "CHARCOAL"
    ),
    "ATHCOAL" = c(
      "HARDCOAL", "BROWN", "ANTCOAL", "COKCOAL",
      "BITCOAL", "SUBCOAL", "OVENCOKE", "GASCOKE",
      "COALTAR", "GASWKSGS", "COKEOVGS", "MANGAS"
    ),
    "ATHLGN" = c("LIGNITE", "PATFUEL", "BKB", "BLFURGS", "OXYSTGS"),
    "ATHOIL" = c("RESFUEL", "NONBIODIES", "LPG", "REFINGAS"),
    "PGSOL" = c("SOLARPV"),
    "PGANUC" = c("NUCLEAR"),
    "PGLHYD" = c("HYDRO"),
    "ATHGAS" = c("NATGAS")
  )

  mapping <- stack(iea_vars)
  colnames(mapping) <- c("product", "variable")

  # group by each technology and sum over its sub-technologies on IEA data
  temp <- iea_data %>%
    left_join(mapping, by = "product") %>%
    rename(variable = "variable.y") %>%
    select(c("region", "period", "value", "variable")) %>%
    group_by(region, period, variable) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    drop_na()

  # Impute missing values in OPEN data using IEA
  z <- open_data %>%
    left_join(temp, by = c("region", "period", "variable")) %>%
    mutate(
      value.x = ifelse(is.na(value.x), value.y, value.x)
    ) %>%
    select(-c("value.y")) %>%
    rename(value = value.x)
  return(z)
}
