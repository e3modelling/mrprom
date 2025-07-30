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

  d <- readSource("IEA2025", subtype = "INDPROD")
  d <- d[,,"KTOE"]
  getItems(d,3.1) <- "Mtoe"
  d <- d / 1000 #ktoe to mtoe
  
  d <- d[,2010:2023,]
  
  df <- data.frame(
    variable = c("HCL", "LGN","CRO","HYD","BMSWAS","NUC","SOL","GEO","WND","NGS"),
    OP = c("ANTHRACITE,COKING_COAL,OTH_BITCOAL", "LIGNITE","CRUDE_OIL",
           "HYDRO","PRIMARY_SOLID_BIOFUEL,BIOGASES","NUCLEAR","SOLAR_PV,SOLAR_THERMAL",
           "GEOTHERMAL","WIND","NATURAL_GAS"))
  
  df <- separate_rows(df, OP, sep = ",")
  
  d <- d[,,c(df[["OP"]])]
  
  d <- collapseDim(d,3.3)
  
  d <- toolAggregate(d,dim = 3.2,rel = df,from = "variable",to = "OP")
  
  qx <- as.quitte(d)
  
  qx <- select(qx, -variable)
  
  names(qx) <- sub("product", "variable", names(qx))
  
  qx <- as.quitte(qx)
  
  # complete incomplete time series
  qx <- qx %>%
    interpolate_missing_periods(period = getYears(d, as.integer = TRUE), expand.values = TRUE)
  
  qx_cro <- qx[which(qx[,4] == "CRO"), ]  %>%
    interpolate_missing_periods(period = 2010 : 2100, expand.values = TRUE)
  
  qx <- rbind(qx, qx_cro[which(qx_cro[,6] > 2023), ])
  
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
