#' readIndustrycosts.R
#'
#' Read data from WSA including the total production, percentage of oxygen and electric production, and DRI production
#'
#' @return The read-in data into a magpie object.
#' 
#' @author Margarita Efthymiadou
#' 
#' @examples
#' \dontrun{
#' a <- readSource("Industrycosts")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter rename select mutate across
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom readxl read_excel
#' @importFrom stringr str_extract
#'
readIndustrycosts <- function() {
  
  # -------------------------
  # DRI Production
  # -------------------------
  capex <- read_excel("i02CostsIndustry.xlsx", sheet = "capex") %>%
    pivot_longer(
      cols = -Technology,
      names_to = "period",
      values_to = "value"
    ) %>%
    mutate(
      period = as.integer(period),
      unit = "Million EUR / Million tonnes",
      variable = "Capex",
      technology = Technology
    ) %>%
    select(technology, period, value, unit, variable)

  opex <- read_excel("i02CostsIndustry.xlsx", sheet = "opex") %>%
    pivot_longer(
      cols = -Technology,
      names_to = "period",
      values_to = "value"
    ) %>%
    mutate(
      period = as.integer(period),
      unit = "Million EUR / Million tonnes",
      variable = "Opex",
      technology = Technology
    ) %>%
    select(technology, period, value, unit, variable)

  om <- read_excel("i02CostsIndustry.xlsx", sheet = "O&M") %>%
    pivot_longer(
      cols = -Technology,
      names_to = "period",
      values_to = "value"
    )%>%
    mutate(
      period = as.integer(period),
      unit = "Million EUR / Million tonnes",
      variable = "O&M",
      technology = Technology
    ) %>%
    select(technology, period, value, unit, variable)
 
  
    
  x <- bind_rows(capex, opex, om) %>%
    as.quitte() %>%
    as.magpie()

    list(x = x,
       weight = NULL,
       description = c(category = "Industry costs",
                       type = "Industry costs",
                       filename = "i02CostsIndustry.xlsx",
                       `Indicative size (MB)` = 0.018,
                       dimensions = "3D",
                       unit = "Million EUR / Million tonnes",
                       Confidential = "E3M"))
}
