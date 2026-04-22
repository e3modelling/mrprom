#' readEURefPlus
#'
#' Read in an xls file and convert it to a magpie object
#' The data has information about GDP|MER and Population.
#'
#' @return magpie object object with the requested output GDP|MER and Population
#'
#' @author Alexandros Tsimpoukis, Dionysis Pramangioulis
#'
#' @examples
#' \dontrun{
#' a <- readSource("EURefPlus")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom tidyr drop_na pivot_longer pivot_wider
#' @importFrom dplyr rename %>% mutate filter select matches arrange group_by summarise
#' @importFrom readxl read_excel read_xlsx
#' @importFrom utils read.csv
#'

readEURefPlus <- function() {

  x <- read_xlsx(
    "MF_EUref2020+_BaselineDataPRIMES_GDP_Pop_clean.xlsx",
    sheet = "Data",
    progress = FALSE
  )

  # Clean column names (camelCase) + year columns like "2015.0" -> "2015"
  x <- x %>%
    rename(
      modelVersion = `Model version`,
      scenario     = Scenario,
      region       = Region,
      variable     = Variable,
      unit         = Units
    )

  # Fix year column names: "2015.0" -> "2015" (keeps non-year columns unchanged)
  names(x) <- gsub("^(\\d{4})\\.0$", "\\1", names(x))

  # Convert to the requested "dummy + regions" format (years as rows)
  yearRegionWide <- x %>%
    select(region, variable, matches("^\\d{4}$")) %>%
    pivot_longer(
      cols      = matches("^\\d{4}$"),
      names_to  = "year",
      values_to = "value"
    ) %>%
    mutate(year = as.integer(year)) %>%
    drop_na(value) %>%
    pivot_wider(
      names_from  = region,
      values_from = value
    ) %>%
    arrange(variable, year) %>%
    rename(dummy = year)

  # Convert to magpie object (region x year x variable)
  xLong <- x %>%
    select(modelVersion, scenario, region, variable, unit, matches("^\\d{4}$")) %>%
    pivot_longer(
      cols      = matches("^\\d{4}$"),
      names_to  = "year",
      values_to = "value"
    ) %>%
    mutate(year = as.integer(year)) %>%
    drop_na(value) %>%
    group_by(modelVersion, scenario, region, variable, unit, year) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")

  xq <- as.quitte(xLong)
  x <- as.magpie(xq)

  list(x = x,
       weight = NULL,
       description = c(category = "demographics",
                       type = "GDP|MER and Population",
                       filename = "MF_EUref2020+_BaselineDataPRIMES_GDP_Pop_clean.xlsx",
                       `Indicative size (MB)` = 0.037,
                       dimensions = "3D",
                       unit = "billion EUR_2020/yr and million"))
}