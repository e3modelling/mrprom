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
  
  x1 <- read.csv("hotmaps_task_2.7_load_profile_residential_heating_yearlong_2010.csv")
  
  x1 <- x1 %>%
  select(NUTS_code = NUTS2_code, hour, load) 

  
  x1 <- x1 %>%
  mutate(region = substr(NUTS_code, 1, 2)) %>%   # AT11 → AT
  group_by(region, hour) %>%
  summarise(load = sum(load, na.rm = TRUE), .groups = "drop")
  
  x <- x1 %>%
  group_by(region) %>%
  summarise(
    average_load = mean(load, na.rm = TRUE),
    maximum_load = max(load, na.rm = TRUE),
    .groups = "drop") %>%
  mutate(value = average_load / maximum_load) %>%
  select(region, value)
  
  x <- x %>%
    pivot_longer(!region, values_to = "value")

  x[["unit"]] <- "%"
  x[["variable"]] <- "avg_heat_load"

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
