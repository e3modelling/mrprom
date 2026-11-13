#' readIndustrycosts.R
#'
#' Read data from costs data (capex,opex,O&M) from EU-CHINA BRIGDE for EAF and BOF.
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
  
 x <- read_excel(
    "i02CostsIndustry.xlsx",
    sheet = "costs"
  ) %>%
    pivot_longer(
      cols = c(capex, opex, OM),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      technology = Technology,
      variable = case_when(
        variable == "capex" ~ "Capex",
        variable == "opex"  ~ "Opex",
        variable == "OM"    ~ "OM"
      ),
      unit = "USD2022 / tonnes crude steel"
    ) %>%
    select(
      technology,
      value,
      unit,
      variable
    ) %>%
    as.quitte() %>%
    as.magpie()

    list(x = x,
       weight = NULL,
       description = c(category = "Industry costs",
                       type = "Industry costs",
                       filename = "i02CostsIndustry.xlsx",
                       `Indicative size (MB)` = 0.018,
                       dimensions = "3D",
                       unit = "USD2022 / tonnes",
                       Confidential = "E3M"))
}
