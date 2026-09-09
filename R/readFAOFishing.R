#' Read FAOSTAT FAOFishing
#'
#' Reads the FAO projections CSV file and converts it into a magpie object.
#'
#' @return A list containing the data as a magpie object and its metadata.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' x <- readSource("FAOFishing")
#' }
#'
#' @importFrom dplyr select mutate filter distinct group_by across summarise %>%
#' @importFrom quitte as.quitte
#' @importFrom utils read.csv
#'
readFAOFishing <- function() {
  
  file <- "Global_production_quantity.csv"
  
  if (!file.exists(file)) {
    stop("FAOSTAT file not found: ", file)
  }
  
  x <- utils::read.csv(
    file,
    check.names = FALSE,
    stringsAsFactors = FALSE
  ) %>%  filter(
    PERIOD %in% c(2010:2100)
  ) %>%  filter(
    MEASURE %in% "Q_tlw" # keep tonnes
  ) %>% select(
    -c(AREA.CODE, PRODUCTION_SOURCE_DET.CODE)
  )
  
  x <- x %>%
    dplyr::select(
      region = COUNTRY.UN_CODE,
      variable = SPECIES.ALPHA_3_CODE,
      period = PERIOD,
      unit = MEASURE,
      value = VALUE
    ) %>%
    dplyr::mutate(
      period = as.integer(period),
      value = as.numeric(value)
    ) # drop status
  
  x <- x %>%
    group_by(region, variable, period, unit) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  
  COUNTRY_GROUPS <- utils::read.csv(
    "CL_FI_COUNTRY_GROUPS.csv",
    check.names = FALSE,
    stringsAsFactors = FALSE
  ) %>% select(
    c(region = UN_Code, Name_En)
  )
  
  data <- left_join(x, COUNTRY_GROUPS, by = "region") %>%
    select(-region)
  
  names(data)[names(data) == "Name_En"] <- "region"
  
  SPECIES_GROUPS <- utils::read.csv(
    "CL_FI_SPECIES_GROUPS.csv",
    check.names = FALSE,
    stringsAsFactors = FALSE
  ) %>% select(
    c(variable = `3A_Code`,Name_En, Yearbook_Group_En)
  ) %>% filter(
    Yearbook_Group_En %in% c("Aquatic animals (Fish, crustaceans and molluscs, etc.)")) %>% select(
                               c(- Yearbook_Group_En))
  
  df <- left_join(data, SPECIES_GROUPS, by = "variable") %>%
    select(-variable)
  
  names(df)[names(df) == "Name_En"] <- "variable"
  
  df[["unit"]] <- "tonnes"
  
  df <- df %>% filter(!is.na(variable))
  
  x <- as.quitte(df)
  x <- as.magpie(x)
  
  return(
    list(x = x,
         weight = NULL,
         description = c(category = "FAOSTAT Fishing",
                         type = "FAOSTAT Fishing",
                         filename = "Global_production_quantity.csv",
                         `Indicative size (MB)` = 55,
                         dimensions = "2D",
                         unit = "various",
                         Confidential = "project")))
}