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

  # load data source (ENERDATA)
  x <- readSource("ENERDATA", "production", convert = TRUE)

  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]

  x <- x[, c(max(fStartHorizon, min(getYears(x, as.integer = TRUE))) : max(getYears(x, as.integer = TRUE))), ]

  # load current OPENPROM set configuration
  sets <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "PGALL")
  sets <- unlist(strsplit(sets[, 1], ","))

  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-enerdata-elecprod-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")

  ## filter mapping to keep only XXX sectors
  map <- filter(map, map[, "PGALL"] %in% sets)
  ## ..and only items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  map <- map[map[, "ENERDATA"] %in% enernames, ]
  ## filter data to keep only XXX data
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  x <- x[, , enernames]
  ## rename variables to openprom names
  getItems(x, 3.1) <- map[map[["ENERDATA"]] %in% paste0(getItems(x, 3.1), ".GWh"), "PGALL"]
  
  # IEA Hydro Plants, replace NA
  b <- readSource("IEA", subtype = "ELOUTPUT")
  b <- as.quitte(b)
  qb <- b
  qb <- filter(qb, qb[["product"]] == "HYDRO")
  qb <- select((qb), c(region, period, value))
  
  qx <- as.quitte(x)
  qx <- left_join(qx, qb, by = c("region", "period"))
  
  qx[which(qx[, 4] == "PGLHYD"),] <- qx[which(qx[, 4] == "PGLHYD"),] %>% mutate(`value.x` = ifelse(is.na(`value.x`), `value.y`, `value.x`))
  names(qx) <- sub("value.x", "value", names(qx))
  qx <- select((qx), -c(`value.y`))
  
  # IEA gas turbine, replace NA
  qn <- b
  qn <- filter(qn, qn[["product"]] == "NATGAS")
  region <- NULL
  period <- NULL
  qn <- select((qn), c(region, period, value))
  
  qn <- mutate(qn, value = sum(value, na.rm = TRUE), .by = c("region", "period")) 
  qn <- distinct(qn)
  
  qx <- left_join(qx, qn, by = c("region", "period"))
  
  qx[which(qx[, 4] == "ACCGT"),] <- qx[which(qx[, 4] == "ACCGT"),] %>% mutate(`value.x` = ifelse(is.na(`value.x`), `value.y`, `value.x`))
  names(qx) <- sub("value.x", "value", names(qx))
  qx <- select((qx), -c(`value.y`))

  # IEA LIGNITE, replace NA
  ql <- b
  ql <- filter(ql, ql[["product"]] == "LIGNITE")
  region <- NULL
  period <- NULL
  ql <- select((ql), c(region, period, value))
  
  ql <- mutate(ql, value = sum(value, na.rm = TRUE), .by = c("region", "period")) 
  ql <- distinct(ql)
  
  qx <- left_join(qx, ql, by = c("region", "period"))
  
  qx[which(qx[, 4] == "ATHLGN"),] <- qx[which(qx[, 4] == "ATHLGN"),] %>% mutate(`value.x` = ifelse(is.na(`value.x`), `value.y`, `value.x`))
  names(qx) <- sub("value.x", "value", names(qx))
  qx <- select((qx), -c(`value.y`))
  
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

  list(x = collapseNames(x),
       weight = NULL,
       unit = getItems(x, 3.2)[1],
       description = "Enerdata; Electricity production")

}
