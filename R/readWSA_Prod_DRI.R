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
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom readxl read_excel
#' @importFrom stringr str_extract
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
      mutate(year = as.integer(year))  %>%
      rename(
        period = 2,
        value = 3
      )
    
    dri[["unit"]] <- "Mt"
    dri[["variable"]] <- "DRI_Production"
    dri <- as.quitte(dri) %>% as.magpie()
    
    prod_sheets <- excel_sheets("WSAdata2019_2023.xlsx")
    prod_sheets <- prod_sheets[grepl("^Prod", prod_sheets)]
    
    shares_list <- vector("list", length(prod_sheets))
    
    for (i in seq_along(prod_sheets)) {
      
      sh <- prod_sheets[i]
      year <- as.integer(stringr::str_extract(sh, "[0-9]{4}"))
      
      shares_list[[i]] <- read_excel(
        "WSAdata2019_2023.xlsx",
        sheet = sh
      ) %>%
        rename(region = 1) %>%
        mutate(year = year)
    }
    
    shares <- bind_rows(shares_list)
    
    # rename columns to desired names
    shares <- shares %>%
      rename(
        Total_Production = contains("Million tonnes"),
        Oxygen     = contains("Oxygen"),
        Electric   = contains("Electric"),
        Open_hearth = contains("Open"),
        Other      = contains("Other"),
        Total      = contains("Total")
      )
    
    shares <- shares %>%
      pivot_longer(
        cols = -c(region, year),
        names_to = "variable",
        values_to = "value"
      ) %>%
      rename(period = year)
    
    shares <- shares %>%
      mutate(
        unit = case_when(
          variable == "Total_Production" ~ "Mt",
          variable == "Oxygen" ~ "%",
          variable == "Electric" ~ "%",
          variable == "Open_hearth" ~ "%",
          variable == "Other" ~ "%",
          variable == "Total" ~ "%",
          TRUE ~ NA_character_
        )
      )
    
    
    shares <- as.quitte(shares) %>% as.magpie()
    
    shares <- as.quitte(shares) %>%
      interpolate_missing_periods(period = 2019 : 2023, expand.values = TRUE)
    
    shares <- as.quitte(shares) %>% as.magpie()
    
    shares <- add_columns(shares, addnm = setdiff(getItems(dri,1),getItems(shares,1)), dim = 1, fill = NA)
    dri <- add_columns(dri, addnm = setdiff(getItems(shares,1),getItems(dri,1)), dim = 1, fill = 0)
    
    x <- mbind(dri, shares)

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
