#' readWorldBankCarPr2025
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
#' a <- readSource("WorldBankCarPr2025")
#' }
#'
#' @importFrom dplyr filter across mutate left_join select rename
#' @importFrom dplyr group_by summarise arrange distinct coalesce all_of
#' @importFrom tidyr pivot_longer complete
#' @importFrom readxl read_excel
#' @importFrom stringr str_detect
#' @importFrom tibble tribble
#'
readWorldBankCarPr2025 <- function() {
  
  # ====== 1. File path ======
  file <- "data_08_2025.xlsx"
  
  # ====== 2. Read historical carbon prices ======
  # Row 1 contains "Data last updated April 1, 2025"
  carbon_price_raw <- readxl::read_excel(
    path = file,
    sheet = "Compliance_Price",
    skip = 1
  )
  
  # ====== 3. Read instrument and jurisdiction information ======
  # The column names are in row 5 in this sheet
  carbon_info_raw <- readxl::read_excel(
    path = file,
    sheet = "Compliance_Gen Info",
    skip = 4
  )
  
  # Keep only the columns needed for matching prices to jurisdictions
  carbon_info <- carbon_info_raw %>%
    dplyr::select(
      `Unique ID`,
      `Instrument name`,
      Type,
      Status,
      `Jurisdiction covered`
    ) %>%
    dplyr::rename(
      initiative_info = `Instrument name`,
      instrument_type_info = Type,
      status = Status,
      country = `Jurisdiction covered`
    ) %>%
    dplyr::distinct(`Unique ID`, .keep_all = TRUE)
  
  # ====== 4. Identify historical year columns ======
  year_cols <- names(carbon_price_raw)[
    stringr::str_detect(names(carbon_price_raw), "^[0-9]{4}$")
  ]
  
  # Keep only years required for the output
  year_cols <- year_cols[
    as.integer(year_cols) >= 2010 &
      as.integer(year_cols) <= 2025
  ]
  
  # ====== 5. Convert price data to long format ======
  carbon_prices_long <- carbon_price_raw %>%
    dplyr::filter(Metric == "US$/tCO2e") %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(year_cols),
      names_to = "year",
      values_to = "price_usd"
    ) %>%
    dplyr::rename(
      unique_id = `Unique ID`,
      initiative = `Name of the initiative`,
      instrument_type = `Instrument Type`
    ) %>%
    dplyr::mutate(
      year = as.integer(year),
      price_usd = suppressWarnings(as.numeric(price_usd))
    )
  
  # ====== 6. Add jurisdiction from Compliance_Gen Info ======
  carbon_info <- carbon_info %>%
    dplyr::rename(unique_id = `Unique ID`)
  
  carbon_prices_long <- carbon_prices_long %>%
    dplyr::left_join(
      carbon_info,
      by = "unique_id"
    ) %>%
    dplyr::mutate(
      # Use the instrument type from Compliance_Price first
      instrument_type = dplyr::coalesce(
        instrument_type,
        instrument_type_info
      ),
      # Use the initiative name from Compliance_Price first
      initiative = dplyr::coalesce(
        initiative,
        initiative_info
      )
    ) %>%
    dplyr::select(
      unique_id,
      country,
      initiative,
      instrument_type,
      status,
      region_group = Region,
      income_group = `Income group`,
      year,
      price_usd
    ) %>%
    dplyr::filter(
      !is.na(country),
      year >= 2010,
      year <= 2025
    )
  
  # ====== 7. Convert nominal USD to constant 2015 USD ======
  cpi_2015 <- data.frame(
    year = 2010:2025,
    cpi_index = c(
      91, 93, 95, 96.5, 98, 100, 101.5, 103,
      105, 107, 108, 110, 112, 114, 116, 118
    )
  )
  
  carbon_prices_long <- carbon_prices_long %>%
    dplyr::left_join(cpi_2015, by = "year") %>%
    dplyr::mutate(
      price_2015usd = price_usd / (cpi_index / 100)
    )
  
  # ====== 8. Map subnational jurisdictions to countries ======
  subnational_to_country <- tibble::tribble(
    ~jurisdiction,          ~country_agg,
    "Alberta",              "Canada",
    "British Columbia",     "Canada",
    "Manitoba",             "Canada",
    "New Brunswick",        "Canada",
    "Newfoundland and Labrador", "Canada",
    "Northwest Territories", "Canada",
    "Nova Scotia",          "Canada",
    "Ontario",              "Canada",
    "Prince Edward Island", "Canada",
    "Quebec",               "Canada",
    "Saskatchewan",         "Canada",
    "California",           "United States",
    "Colorado",             "United States",
    "Connecticut",          "United States",
    "Delaware",             "United States",
    "Hawaii",               "United States",
    "Maine",                "United States",
    "Maryland",             "United States",
    "Massachusetts",        "United States",
    "New Jersey",           "United States",
    "New York",             "United States",
    "Oregon",               "United States",
    "Rhode Island",         "United States",
    "Vermont",              "United States",
    "Virginia",             "United States",
    "Washington",           "United States",
    "Beijing",              "China",
    "Chongqing",            "China",
    "Fujian",               "China",
    "Guangdong",            "China",
    "Hubei",                "China",
    "Shanghai",             "China",
    "Shenzhen",             "China",
    "Tianjin",              "China",
    "Tokyo",                "Japan",
    "Saitama",              "Japan",
    "Baja California",      "Mexico",
    "Guanajuato",           "Mexico",
    "Jalisco",              "Mexico",
    "Queretaro",            "Mexico",
    "State of Mexico",      "Mexico",
    "Tamaulipas",           "Mexico",
    "Yucatan",              "Mexico",
    "Zacatecas",            "Mexico"
  )
  
  carbon_prices_long <- carbon_prices_long %>%
    dplyr::left_join(
      subnational_to_country,
      by = c("country" = "jurisdiction")
    ) %>%
    dplyr::mutate(
      country_agg = dplyr::coalesce(country_agg, country)
    )
  
  # ====== 9. Summarize by country and year ======
  carbon_prices_summary <- carbon_prices_long %>%
    dplyr::group_by(
      country = country_agg,
      year
    ) %>%
    dplyr::summarise(
      avg_price_usd_nominal = {
        valid_prices <- price_usd[
          !is.na(price_usd) &
            price_usd > 0
        ]
        
        if (length(valid_prices) == 0) {
          0
        } else {
          mean(valid_prices)
        }
      },
      
      avg_price_2015usd = {
        valid_prices <- price_2015usd[
          !is.na(price_2015usd) &
            price_2015usd > 0
        ]
        
        if (length(valid_prices) == 0) {
          0
        } else {
          mean(valid_prices)
        }
      },
      
      n_systems = sum(
        !is.na(price_usd) &
          price_usd > 0
      ),
      
      instrument_types = paste(
        sort(unique(
          instrument_type[
            !is.na(price_usd) &
              price_usd > 0 &
              !is.na(instrument_type)
          ]
        )),
        collapse = "; "
      ),
      
      .groups = "drop"
    ) %>%
    dplyr::arrange(country, year)
  
  # ====== 10. Complete missing years for each country ======
  carbon_prices_summary <- carbon_prices_summary %>%
    tidyr::complete(
      country,
      year = 2010:2025,
      fill = list(
        avg_price_usd_nominal = 0,
        avg_price_2015usd = 0,
        n_systems = 0,
        instrument_types = ""
      )
    )
  
  # ====== 11. Convert country names to ISO codes ======
  carbon_prices_summary[["country"]] <- toolCountry2isocode(
    carbon_prices_summary[["country"]],
    mapping = c(
      "Korea, Rep." = "KOR",
      "Republic of Korea" = "KOR",
      "South Korea" = "KOR",
      "Russia" = "RUS",
      "Taiwan, China" = "TWN",
      "Türkiye" = "TUR",
      "United States" = "USA",
      "EU27+" = "EU"
    )
  )
  
  # ====== 12. Format output ======
  carbon_prices_summary <- carbon_prices_summary %>%
    dplyr::rename(
      region = country,
      period = year,
      value = avg_price_2015usd
    ) %>%
    dplyr::mutate(
      unit = "US$2015/t CO2",
      variable = "Price|Carbon"
    ) %>%
    dplyr::filter(
      period >= 2010,
      period <= 2025,
      !is.na(region)
    )
  
  # ====== 13. Convert to quitte and magpie ======
  x <- carbon_prices_summary %>%
    dplyr::select(
      region,
      period,
      variable,
      unit,
      value
    )
  
  x <- quitte::as.quitte(x)
  x <- magclass::as.magpie(x)
  
  
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
