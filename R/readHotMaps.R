#' readHotMaps
#'
#' Read Residential Heating Loads from Hotmaps
#'
#' @return The read-in data into a magpie object.
#'
#' @author Margarita Efthymiadou
#'
#' @examples
#' \dontrun{
#' a <- readSource("HotMaps")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter rename select mutate across
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom quitte as.quitte
#' @importFrom readxl read_excel
#'
readHotMaps <- function() {
  
  x <- read.csv("hotmaps_task_2.7_load_profile_residential_heating_yearlong_2010.csv")
  
  x <- x %>%
  select(NUTS_code = NUTS2_code, hour, datetime, load) %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y/%m/%d %H:%M:%S"))
  
  x <- x %>%
  rename(
    region = NUTS_code,
    value = load)
  head(x)
  
  
  x[["unit"]] <- "MW"
  x[["variable"]] <- "heat_load"
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "heat_load",
                       type = "heat_load",
                       filename = "hotmaps_task_2.7_load_profile_residential_heating_yearlong_2010.csv",
                       `Indicative size (MB)` = 310,
                       dimensions = "3D", ###
                       unit = "MW",
                       Confidential = "E3M"))
}
