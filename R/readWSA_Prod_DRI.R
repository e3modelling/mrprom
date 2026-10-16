#' readWSA_Prod_DRI
#'
#' Read data from WSA including the total production, percentage of oxygen and electric production, and DRI production
#'
#' @return The read-in data into a magpie object.
#'
#' @author Margarita Efthymiadou
#'
#' @examples
#' \dontrun{
#' a <- readSource("WSA_Prod_DRI")
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter rename select mutate across
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom quitte as.quitte
#' @importFrom readxl read_excel
#' @importFrom purrr map_df
#'
readWSA_Prod_DRI <- function() {
  
  # -------------------------
  # DRI Production
  # -------------------------
  dri <- read_excel("WSAdata2019_2023.xlsx", sheet = "DRI Production") %>%
    rename(region = 1) %>%
    pivot_longer(
      -region,
      names_to = "year",
      values_to = "DRI_Production"
    ) %>%
    mutate(year = as.integer(year))

  # -------------------------
  # Production shares
  # -------------------------
  prod_sheets <- excel_sheets("WSAdata2019_2023.xlsx") %>%
    .[grepl("^Prod", .)]

  shares <- map_df(prod_sheets, function(sh) {

    year <- str_extract(sh, "[0-9]{4}")

    tmp <- read_excel("WSAdata2019_2023.xlsx", sheet = sh)

    tmp %>%
      rename(region = 1) %>%
      mutate(year = as.integer(year))
  })

  # rename columns to desired names
  shares <- shares %>%
    rename(
      Total_Production = contains("Million tonnes"),
      Oxygen_pct     = contains("Oxygen"),
      Electric_pct   = contains("Electric"),
      Open_hearth_pct = contains("Open"),
      Other_pct      = contains("Other"),
      Total_pct      = contains("Total")
    )

  # -------------------------
  # Merge
  # -------------------------
  df <- full_join(
    dri,
    shares,
    by = c("region", "year")
  ) %>%
    mutate(
      across(
        c(DRI_Production,
          Total_Production,
          Oxygen_pct,
          Electric_pct,
          Open_hearth_pct,
          Other_pct,
          Total_pct),
        ~replace_na(., 0)
      )
  ) %>%
  arrange(region, year)

  df_long <- df %>%
    pivot_longer(
      cols = c(
        DRI_Production,
        Total_Production,
        Oxygen_pct,
        Electric_pct,
        Open_hearth_pct,
        Other_pct,
        Total_pct
        ),
      names_to = "variable",
      values_to = "value"
  ) %>%
  rename(period = year)

  df_long <- df_long %>%
  mutate(
    unit = case_when(
      variable == "DRI_Production" ~ "Mt",
      variable == "Total_Production" ~ "Mt",
      variable == "Oxygen_pct" ~ "%",
      variable == "Electric_pct" ~ "%",
      variable == "Open_hearth_pct" ~ "%",
      variable == "Other_pct" ~ "%",
      variable == "Total_pct" ~ "%",
      TRUE ~ NA_character_
    )
  )
  
  x <- as.quitte(df_long)
  x <- as.magpie(x)
  
    list(x = x,
       weight = NULL,
       description = c(category = "Steel production",
                       type = "Steel production",
                       filename = "WSAdata2019_2023.xlsx",
                       `Indicative size (MB)` = 0.34,
                       dimensions = "3D",
                       unit = "Million tonnes/yr or %",
                       Confidential = "E3M"))
}
