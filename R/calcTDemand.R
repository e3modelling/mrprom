#' calcTDemand
#'
#' Use Primes, IEA data for Secondary Energy Electricity per year.
#' Info:
#' Primes: Trends for Secondary Energy Electricity data, EU countries until 2070
#' IEA: Trends for Secondary Energy Electricity data, 225 countries until 2050.
#' The trends are the same for each country depending to the region. For example
#' HKG and CHN have the same trends for capacity, 225 countries until 2050.
#' IEA mapping: "Africa" = "SSA", "Middle East" = "MEA", "Eurasia" = "REF",
#' "Southeast Asia" = "OAS", "Central and South America" = "LAM",
#' "Asia Pacific" = "CAZ", "Europe" = "NEU", "European Union" = "ELL"
#' calculate CAZ, NEU and ELL 
#' "CAZ" <- "CAZ" -  "OAS"
#' ELL and NEU have the same trends
#' IEA_non_EU <- "NEU" - "ELL"
#' "NEU" <- IEA_non_EU
#' "ELL" <- IEA_non_EU
#' The trends are multiplied with the
#' calcOutput(type = "IDataElecProd", mode = "Total", aggregate = FALSE) 
#' data to find the Secondary Energy Electricity data.
#' 
#' @return Secondary Energy Electricity per year
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "TDemand", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr filter %>% mutate select full_join arrange group_by distinct intersect setdiff ungroup group_map 
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom utils tail


calcTDemand <- function() {
  
  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  
  #Primes Secondary Energy Electricity data
  a <- readSource("PrimesPGData", subtype = "SE")
  
  a <- a[getRegions(a)[getRegions(a) %in% as.character(getISOlist())], , ]
  
  a <-  as.quitte(a) %>%
    interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
  
  a <- as.quitte(a) %>% as.magpie()
  
  Primes <- a[,fStartHorizon:max(getYears(a, as.integer = TRUE)),]
  
  map <- toolGetMapping("regionmappingOPDEV5.csv", "regional", where = "mrprom")
  
  Primes <- Primes[intersect(getRegions(Primes),map[,3]),,]
  Primes <- collapseDim(Primes, 3.1)
  
  #calculate trend = difference between two years devided by previous year
  Primes <- as.quitte(Primes) %>%
    arrange(region, period) %>%   # Ensure sorting within each region
    group_by(region) %>%
    mutate(
      prev_value = lag(value),
      diff_ratio = (value - prev_value) / prev_value
    )
  
  Primes <- select(Primes,"region","variable","unit","period","diff_ratio")
  
  names(Primes) <- sub("diff_ratio", "value", names(Primes))
  
  #set trend equal to 2070 after this year
  Primes <- Primes %>%
    group_by(region) %>%
    mutate(
      value_2070 = value[period == 2070][1],  # grab value for 2070 in each region
      value = ifelse(period > 2070, value_2070, value)
    ) %>%
    select(-value_2070) %>%
    ungroup()
  
  Primes <- as.quitte(Primes) %>% as.magpie()
  
  #a <- toolCountryFill(a, fill = NA)
  
  #filter navigate data by variable
  # x <- readSource("Navigate", subtype = "SUP_NPi_Default", convert = FALSE)
  # 
  # x <- x[,,"Secondary Energy|Electricity"][,,"REMIND-MAgPIE 3_2-4_6"]
  # 
  # x <- as.quitte(x)
  # 
  # x[["region"]] <- toolCountry2isocode((x[["region"]]), mapping =
  #                                        c("R9CHINA" = "CHN",
  #                                          "R9INDIA" = "IND",
  #                                          "R9USA" = "USA",
  #                                          "REMIND 3_2|India" = "IND",
  #                                          "REMIND 3_2|Japan" = "JPN",
  #                                          "REMIND 3_2|United States of America" = "USA",
  #                                          "REMIND 3_2|Russia and Reforming Economies" = "RUS"))
  # x <- filter(x, !is.na(x[["region"]]))
  # x <- filter(x, !is.na(x[["value"]]))
  # x <- distinct(x)
  # x <- as.quitte(x)
  # # #take the mean value from the available models
  # # x <- mutate(x, value = mean(value, na.rm = TRUE), .by = c("scenario", "region", "period", "variable", "unit"))
  # # 
  # x[["model"]] <- "(Missing)"
  # 
  # x <- unique(x)
  # 
  # x <- as.quitte(x) %>% as.magpie()
  # 
  # x<- x * 277.7778 #EJ to TWh
  # 
  # getItems(x,3.3) <- "TWh"
  # 
  # x <-  as.quitte(x) %>%
  #   interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
  # 
  # x <- as.quitte(x) %>% as.magpie()
  # 
  # x <- toolCountryFill(x, fill = NA)
  # 
  # x_Navigate <- x[,fStartHorizon : 2100,]
  # 
  # #remove model and scenario dimension
  # a <- collapseDim(a,3.1)
  # x_Navigate <- collapseDim(x_Navigate,3.1)
  # 
  # a_primes <- as.quitte(a)
  # b_navigate <- as.quitte(x_Navigate)
  
  #IEA SE
  IEA_WEO_2025 <- readSource("IEA_WEO_2025_ExtendedData", subtype = "IEA_WEO_2025_ExtendedData")
  max_IEA_years <- max(getYears(IEA_WEO_2025, as.integer = TRUE))
  
  IEA_Historical <- IEA_WEO_2025[,,"Electricity generation"][,,"Historical"][,,"TWh"][,,"Total"][,,"Total"]
  IEA_Historical <- collapseDim(IEA_Historical,3)
  
  IEA_WEO_2025 <- IEA_WEO_2025[,,"Electricity generation"][,,"Stated Policies Scenario"][,,"TWh"][,,"Total"]
  IEA_WEO_2025 <- collapseDim(IEA_WEO_2025,3)
  
  IEA_WEO_2025 <- mbind(IEA_Historical[,c(2010,2015,2023,2024),], IEA_WEO_2025[,c(2035,2040,2045,2050),])
  
  IEA_WEO_2025 <- add_dimension(IEA_WEO_2025, dim = 3.1, add = "variable", nm = "Secondary Energy|Electricity")
  IEA_WEO_2025 <- add_dimension(IEA_WEO_2025, dim = 3.2, add = "unit", nm = "TWh")
  
  IEA_WEO_2025 <- as.quitte(IEA_WEO_2025)
  
  IEA_WEO_2025[["region"]] <- toolCountry2isocode((IEA_WEO_2025[["region"]]), mapping =
                                                    c("Africa" = "SSA",
                                                      "Middle East" = "MEA",
                                                      "Eurasia" = "REF",
                                                      "Southeast Asia" = "OAS",
                                                      "Central and South America" = "LAM",
                                                      "Asia Pacific" = "CAZ",
                                                      "Europe" = "NEU",
                                                      "European Union" = "ELL"))
  
  IEA_WEO_2025 <- filter(IEA_WEO_2025, !is.na(IEA_WEO_2025[["region"]]))
  IEA_WEO_2025 <- filter(IEA_WEO_2025, !is.na(IEA_WEO_2025[["value"]]))
  IEA_WEO_2025 <- distinct(IEA_WEO_2025)
  IEA_WEO_2025 <- as.quitte(IEA_WEO_2025) %>% 
    interpolate_missing_periods(period = fStartHorizon : 2100, expand.values = TRUE)
  
  IEA_WEO_2025 <- as.quitte(IEA_WEO_2025) %>% as.magpie()
  
  #calculate CAZ,NEU and ELL value
  IEA_WEO_2025["CAZ",,] <- IEA_WEO_2025["CAZ",,] - IEA_WEO_2025["OAS",,]
  
  #ELL and NEU have the same trends
  IEA_non_EU <- IEA_WEO_2025["NEU",,] - IEA_WEO_2025["ELL",,]
  IEA_WEO_2025["NEU",,] <- IEA_non_EU
  
  map_IEA <- filter(map, Region.Code %in% getRegions(IEA_WEO_2025))
  
  #set SE of countries equal to their regions
  IEA <-  toolAggregate(IEA_WEO_2025[unique(map_IEA[,"Region.Code"]),,], rel = map_IEA,  from = "Region.Code", to = "ISO3.Code", weight = NULL)
  
  #calculate region CHA
  IEA_CHA <- IEA_WEO_2025["CHN",,]
  
  IEA_CHA <- add_columns(IEA_CHA, addnm = "HKG", dim = 1, fill = NA)
  IEA_CHA <- add_columns(IEA_CHA, addnm = "MAC", dim = 1, fill = NA)
  IEA_CHA <- add_columns(IEA_CHA, addnm = "TWN", dim = 1, fill = NA)
  
  IEA_CHA["HKG",,] <- IEA_CHA["CHN",,]
  IEA_CHA["MAC",,] <- IEA_CHA["CHN",,]
  IEA_CHA["TWN",,] <- IEA_CHA["CHN",,]
  
  IEA <- mbind(IEA, IEA_CHA)
  
  #find trend of Secondary energy electricity
  IEA <- as.quitte(IEA) %>%
    arrange(region, period) %>%   # Ensure sorting within each region
    group_by(region) %>%
    mutate(
      prev_value = lag(value),
      diff_ratio = (value - prev_value) / prev_value
    )
  
  IEA <- select(IEA,"region","variable","unit","period","diff_ratio")
  
  names(IEA) <- sub("diff_ratio", "value", names(IEA))
  
  #set trend equal to 2050 after this year
  IEA <- IEA %>%
    group_by(region) %>%
    mutate(
      value_2070 = value[period == 2050][1],  # grab value for 2070 in each region
      value = ifelse(period > 2050, value_2070, value)
    ) %>%
    select(-value_2070) %>%
    ungroup()
  
  IEA <- as.quitte(IEA) %>% as.magpie()
  
  #combine two datasets
  x <- mbind(IEA, Primes)
 
  #2010 is NA and set equal to 2011
  x[,2010,] <- x[,2011,]
  
  a <- calcOutput(type = "IDataElecProd", mode = "Total", aggregate = FALSE)
  
  a <- dimSums(a, 3, na.rm = TRUE)
  
  years <- setdiff(getYears(x), getYears(a))
  
  years_int <- setdiff(getYears(x, as.integer = TRUE), getYears(a, as.integer = TRUE))
  
  a <- add_columns(a, addnm = years, dim = 2, fill = NA)
  
  x <- collapseDim(x,3)
  
  qa <- as.quitte(a)
  qx <- as.quitte(x)
  
  df <- qa %>%
    left_join(qx, by = c("model","scenario","region","variable","unit","period"))
  
  
  names(df) <- sub("value.x", "value", names(df))
  names(df) <- sub("value.y", "multiplier", names(df))
  
  
  df_updated <- df %>%
    group_by(region) %>%
    arrange(period) %>%
    group_modify(~ {
      d <- .x
      start <- which(!is.na(d$value))[1]
      if (!is.na(start)) {
        for (i in (start + 1):nrow(d)) {
          d$value[i] <- d$value[i - 1] + d$value[i - 1] * d$multiplier[i - 1]
        }
      }
      d
    }) %>%
    ungroup()
  
  df_updated <- select(df_updated, "region","model","scenario","period","value")
  
  a <- as.quitte(df_updated) %>% as.magpie()
  
  a <- a / 1000 #fix units
  
  a <- collapseDim(a, 3)
  
  a <- add_dimension(a, dim = 3.1, add = "variable", nm = "Secondary Energy|Electricity")
  a <- add_dimension(a, dim = 3.2, add = "unit", nm = "TWh")
  
  list(x = a,
       weight = NULL,
       unit = "TWh",
       description = "Primes,IEA Secondary Energy Electricity per year")
  
}