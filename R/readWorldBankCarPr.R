#' readWorldBankCarPr
#'
#' Read in data from the World Bank project.
#' The dataset contains carbon price data.
#'
#'
#' @return The read-in carbon price data into a magpie object
#'
#' @author Anastasis Giannousakis
#'
#' @examples
#' \dontrun{
#' a <- readSource("WorldBankCarPr")
#' }
#'
#' @importFrom dplyr filter across mutate left_join select
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel
#' @importFrom stringr str_detect
#'
readWorldBankCarPr <- function() {
  
  # ====== 1. File paths ======
  file <- "data-latest.xlsx"
  
  # ====== 2. Read Compliance_Price sheet ======
  carbon_price_raw <- read_excel(file, sheet = "Compliance_Price", skip = 1)
  
  # ====== 3. Identify year columns ======
  year_cols <- names(carbon_price_raw)[str_detect(names(carbon_price_raw), "^[0-9]{4}$")]
  
  # ====== 4. Convert year columns to numeric ======
  carbon_price_raw <- carbon_price_raw %>%
    mutate(across(all_of(year_cols), ~as.numeric(.)))
  
  # ====== 5. Pivot to long format ======
  carbon_prices_long <- carbon_price_raw %>%
    pivot_longer(
      cols = all_of(year_cols),
      names_to = "year",
      values_to = "price_usd"
    ) %>%
    rename(
      country = `Jurisdiction Covered`,
      initiative = `Name of the initiative`,
      instrument_type = `Instrument Type`
    ) %>%
    mutate(
      year = as.numeric(year),
      price_usd = as.numeric(price_usd)
    ) %>%
    filter(year <= 2025)
  
  # ====== 6. Convert to 2015 USD ======
  cpi_2015 <- data.frame(
    year = 2010:2025,
    cpi_index = c(91, 93, 95, 96.5, 98, 100, 101.5, 103, 105, 107, 108, 110, 112, 114, 116, 118)
  )
  
  carbon_prices_long <- carbon_prices_long %>%
    left_join(cpi_2015, by = "year") %>%
    mutate(price_2015usd = price_usd / (cpi_index / 100))
  
  # ====== 7. Subnational to national mapping ======
  # Add all known subnational regions
  subnational_to_country <- data.frame(
    jurisdiction = c("California", "Quebec", "British Columbia", "New Zealand ETS", "Tokyo Metropolis"), # extend as needed
    country_agg = c("USA", "Canada", "Canada", "New Zealand", "Japan") # corresponding national level
  )
  
  carbon_prices_long <- carbon_prices_long %>%
    left_join(subnational_to_country, by = c("country" = "jurisdiction")) %>%
    mutate(country_agg = ifelse(is.na(country_agg), country, country_agg))
  
  # ====== 8. Summarize per country-year ======
  carbon_prices_summary <- carbon_prices_long %>%
    group_by(country = country_agg, year) %>%
    summarise(
      avg_price_usd_nominal = mean(price_usd[price_usd > 0], na.rm = TRUE),
      avg_price_2015usd = mean(price_2015usd[price_usd > 0], na.rm = TRUE),
      n_systems = sum(price_usd > 0, na.rm = TRUE),
      instrument_types = paste(unique(instrument_type[price_usd > 0]), collapse = "; "),
      .groups = "drop"
    ) %>%
    # fill zeros for countries/years without initiatives
    mutate(
      avg_price_usd_nominal = ifelse(is.na(avg_price_usd_nominal), 0, avg_price_usd_nominal),
      avg_price_2015usd = ifelse(is.na(avg_price_2015usd), 0, avg_price_2015usd),
      n_systems = ifelse(is.na(n_systems), 0, n_systems),
      instrument_types = ifelse(is.na(instrument_types), "", instrument_types)
    ) %>%
    arrange(country, year)
  
  carbon_prices_summary[["country"]] <- toolCountry2isocode(carbon_prices_summary[["country"]], mapping =
                                                              c("Korea, Rep." = "KOR",
                                                                "State of Mexico" = "MEX",
                                                                "Taiwan, China" = "TWN"))
  
  names(carbon_prices_summary) <- sub("country","region",names(carbon_prices_summary))
  names(carbon_prices_summary) <- sub("year","period",names(carbon_prices_summary))
  names(carbon_prices_summary) <- sub("avg_price_2015usd","value",names(carbon_prices_summary))
  carbon_prices_summary[["unit"]] <- "US$2015/t CO2"
  carbon_prices_summary[["variable"]] <- "Price|Carbon"
  carbon_prices_summary <- filter(carbon_prices_summary, period >= 2010 & period <= 2025)
  carbon_prices_summary <- filter(carbon_prices_summary, !is.na(region))
  
  x <- select(carbon_prices_summary,c(region, period, variable, value))
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  
  list(x = x,
       weight = NULL,
       description = c(category = "Costs",
                       type = "Carbon Price",
                       filename = "data-latest.xlsx",
                       `Indicative size (MB)` = 0.26,
                       dimensions = "4D",
                       unit = "varius",
                       Confidential = "project"))

}
