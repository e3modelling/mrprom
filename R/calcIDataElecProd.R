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
#' @importFrom tidyr pivot_wider
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
  x <- x[, , enernames]
  ## rename variables to openprom names
  getItems(x, 3.1) <- map[map[["ENERDATA"]] %in% paste0(getItems(x, 3.1), ".GWh"), "PGALL"][1:12]

  # share of PV, CSP
  share_of_PV <- share_of_solar[, , "PGSOL"] / (share_of_solar[, , "PGASOL"] + share_of_solar[, , "PGSOL"])
  share_of_CSP <- share_of_solar[, , "PGASOL"] / (share_of_solar[, , "PGASOL"] + share_of_solar[, , "PGSOL"])
  x_CSP <- x[, , "PGSOL"] * ifelse(is.na(share_of_CSP), mean(share_of_CSP, na.rm = TRUE), share_of_CSP)
  x[, , "PGSOL"] <- x[, , "PGSOL"] * ifelse(is.na(share_of_PV), mean(share_of_PV, na.rm = TRUE), share_of_PV)
  x_CSP <- collapseDim(x_CSP, 3.3)
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
      value.x = ifelse(variable == "ACCGT" & is.na(value.x), value.y, value.x)
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
  x <- as.quitte(qx) %>% as.magpie()
  # set NA to 0
  x[is.na(x)] <- 0

  list(
    x = collapseNames(x),
    weight = NULL,
    unit = getItems(x, 3.2)[1],
    description = "Enerdata; Electricity production"
  )
}
