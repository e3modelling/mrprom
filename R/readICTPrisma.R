#' readICTPrisma
#'
#' @return The read-in data into a magpie object.
#' 
#'  @param subtype By choosing a subtype you can select the corresponding csv
#' file and convert it to a magpie object.
#' Available types are:
#' \itemize{
#' \item `Number of data centers`:
#' \item `Consumption of data centers`:
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("ICTPrisma")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter select
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#' @importFrom stringr str_extract str_remove
#'
readICTPrisma <- function(subtype) {
  
  if (subtype == "Number of data centers") {
    x <- read_excel("R12_Clean IAM Version_Finalised.xlsx",
                    sheet = "Num. DC", skip = 1)
    
    x <- x[-c(1,2,50),-c(3,6,9,12,15,18:length(x))]
    
    odd_values  <- as.vector(as.matrix(x[, seq(2, ncol(x), by = 2)]))
    even_values <- as.vector(as.matrix(x[, seq(1, ncol(x), by = 2)]))
    
    df <- data.frame(
      odd_columns  = odd_values,
      even_columns = even_values
    )
    
    names(df) <- c("value", "region")
    
    df <- df[!is.na(df$region) & !grepl("^[0-9]+(\\.[0-9]+)?$", df$region), ]
    
    df[["variable"]] <- "Number of data centers"
    
    df[["unit"]] <- "number"
    
    df <- df[!duplicated(df), ]
    
    x <- as.quitte(df) %>% as.magpie()
  }

  if (subtype == "Consumption of data centers") {
    x <- read_excel("R12_Clean IAM Version_Finalised.xlsx",
                    sheet = "Num. DC", skip = 2)
    
    x <- select(x, c("Europe","Asia Pacific", "North America", "South America",
                     "Africa", "Oceania"))
    
    df <- x %>%
      pivot_longer(
        cols = everything(),
        names_to = "region",
        values_to = "country"
      ) %>%
      filter(
        !is.na(country),                 # remove NA
        !grepl("^\\d+$", country)        # remove purely numeric values like "3"
      )
    
    # 1. Count how many countries per region
    region_size <- df %>%
      group_by(region) %>%
      summarise(n_countries = n(), .groups = "drop")
    
    # 2. Join this info back to original df
    df_clean <- df %>%
      left_join(region_size, by = "region") %>%
      
      # 3. For each country, keep the region with the max size
      group_by(country) %>%
      slice_max(n_countries, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      
      # 4. Drop helper column
      select(region, country)
    
    map <- df_clean
    
    x <- read_excel("R12_Clean IAM Version_Finalised.xlsx",
                    sheet = "R5 DC")
    
    names(x) <- gsub("Region", "region", names(x))
    names(x) <- gsub("Year", "period", names(x))
    
    df_long <- x %>%
      pivot_longer(
        cols = -c(region, Scenario, period),   # keep these
        names_to = "variable",
        values_to = "value"
      )
    
    x <- as.quitte(df_long) %>% as.magpie()
    
    Numberdatacenters <- readSource("ICTPrisma", subtype = "Number of data centers")
    Numberdatacenters <- collapseDim(Numberdatacenters, 3)
    
    map <- map %>%
      mutate(region = case_when(
        region == "Asia Pacific"  ~ "Asia Pacific",
        region == "Europe" ~ "Europe",
        region == "South America"        ~ "Latin America",
        region == "Africa"        ~ "Middle East and Africa",
        region == "North America"       ~ "North America",
        TRUE ~ region
      ))
    
    x <- toolAggregate(x, weight = Numberdatacenters, rel = map, from = "region", to = "country")
    
  }
  
  
  list(
    x = x,
    weight = NULL,
    description = c(
      category = "Data_Centers",
      type = "Data_Centers",
      filename = "R12_Clean IAM Version_Finalised.xlsx",
      `Indicative size (MB)` = 0.792,
      dimensions = "3D",
      unit = "various",
      Confidential = "E3M"
    )
  )
}
