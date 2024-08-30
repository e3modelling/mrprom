#' calcISuppPrimprod
#'
#' Use ENERDATA Primary production data to derive OPENPROM input parameter iSuppPrimProd.
#'
#' @return  OPENPROM input data iSuppPrimProd.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "ISuppPrimprod", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom quitte as.quitte


calcISuppPrimprod <- function() {

  # load data source (ENERDATA)
  x <- readSource("ENERDATA", "production", convert = TRUE)

  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]

  x <- x[, c(max(fStartHorizon, min(getYears(x, as.integer = TRUE))) : max(getYears(x, as.integer = TRUE))), ]

  # load current OPENPROM set configuration
  sets <- toolreadSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "PPRODEF")
  sets <- unlist(strsplit(sets[, 1], ","))

  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-enerdata-primaryproduction-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")
  z <- map[["EF"]]
  ## filter mapping
  map <- filter(map, map[, "EF"] %in% sets)
  ## ..and only items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  map <- map[map[, "ENERDATA"] %in% enernames, ]
  ## filter data
  enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
  x <- x[, , enernames]
  ## rename variables to openprom names
  getItems(x, 3.1) <- map[map[["ENERDATA"]] %in% paste0(getItems(x, 3.1), ".Mtoe"), "EF"]

  promnames <- subset(z, !(z %in% getItems(x, 3.1)))

  # Adding the PROM variables with placeholder values
  for (name in promnames) {
    x <- add_columns(x, addnm = name, dim = "variable", fill = 0.00000001)
  }

  qx <- as.quitte(x)
  
  # IEA HCL
  region <- NULL
  period <- NULL
  value <- NULL
  b <- readSource("IEA", subtype = "INDPROD") / 1000 #ktoe to Mtoe
  b <- as.quitte(b) 
  iea <- b
  qb <- b
  qb <- filter(qb, qb[["product"]] %in% c("BITCOAL", "COKCOAL", "ANTCOAL"))
  qb <- select((qb), c(region, period, value))
  qb <- mutate(qb, value = sum(value, na.rm = TRUE), .by = c("period", "region"))
  qb <- distinct(qb)
  qx <- left_join(qx, qb, by = c("region", "period"))
  
  qx[which(qx[, 4] == "HCL"),] <- qx[which(qx[, 4] == "HCL"),] %>% mutate(`value.x` = ifelse(is.na(`value.y`), `value.x`, `value.y`))
  names(qx) <- sub("value.x", "value", names(qx))
  qx <- select((qx), -c(`value.y`))
  
  # IEA, LIGNITE
  qb <- iea
  qb <- filter(qb, qb[["product"]] == "LIGNITE")
  qb <- select((qb), c(region, period, value))
  
  qx <- left_join(qx, qb, by = c("region", "period"))
  
  qx[which(qx[, 4] == "LGN"),] <- qx[which(qx[, 4] == "LGN"),] %>% mutate(`value.x` = ifelse(is.na(`value.y`), `value.x`, `value.y`))
  names(qx) <- sub("value.x", "value", names(qx))
  qx <- select((qx), -c(`value.y`))
  
  #if LGN 1e-08 take the value of HCL and multiply by share LGN/HCL
  share_LGN_HCL <- qx[which(qx[, 4] == "LGN" & qx[, 3] == "USA"), 7] / qx[which(qx[, 4] == "HCL" & qx[, 3] == "USA"), 7]
  quitte_share_LGN_HCL <- qx[which(qx[, 4] == "LGN" & qx[, 3] == "USA"),]
  quitte_share_LGN_HCL[, 7] <- share_LGN_HCL
  quitte_share_LGN_HCL[, 4] <- "HCL"
  
  value <- NULL
  value.x <- NULL
  value.y <- NULL
  qx_c <- left_join(qx, quitte_share_LGN_HCL, by = c("variable", "period", "unit",  "model",  "scenario"))
  qx_c[which(qx_c[, 4] == "LGN"), 7] <- qx_c[which(qx_c[, 4] == "HCL"), 7] * qx_c[which(qx_c[, 4] == "HCL"), 9]
  names(qx_c) <- sub("region.x", "region", names(qx_c))
  names(qx_c) <- sub("value.x", "value", names(qx_c))
  qx_c <- select(qx_c, -c("region.y", "value.y"))
  
  qx_d <- left_join(qx, qx_c, by = c("variable", "period", "unit", "region", "model",  "scenario"))
  qx_d[which(qx_d[, 4] == "LGN" & qx_d[, 7] == 1e-08), 7] <- qx_d[which(qx_d[, 4] == "LGN" & qx_d[, 7] == 1e-08), 8]
  
  names(qx_d) <- sub("value.x", "value", names(qx_d))
  qx_d <- select(qx_d, -c("value.y"))
  
  qx <- qx_d
  
  # complete incomplete time series
  qx <- qx %>%
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
       description = "Enerdata; Primary production")

}
