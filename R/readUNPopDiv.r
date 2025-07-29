#' readUNPopDiv
#' 
#' Read data (historical + estimates for medium variant) from the UN World Population Prospects 2024
#' 
#' @return The read-in data into a magpie object
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Alexandros Tsimpoukis 
#'
#' @examples
#' \dontrun{
#' a <- readSource("UNPopDiv", subtype =  "Bangladesh", convert = FALSE)
#' }
#' @importFrom readxl read_excel
#' @importFrom dplyr filter select mutate

readUNPopDiv <- function(subtype =  "Bangladesh") {


  inputFile <- "WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_COMPACT.xlsx"
  estimates <- read_excel(inputFile, sheet = "Estimates", skip = 16, guess_max = 10000)
  mediumVariant <- read_excel(inputFile, sheet = "Medium variant", skip = 16, guess_max = 10000)

  # rename column name for brevity
  estimates <- estimates %>%
    rename(Location = `Region, subregion, country or area *`)

  mediumVariant <- mediumVariant %>%
    rename(Location = `Region, subregion, country or area *`)

  # if there is a subtype, make sure it's character and split it
  if (!is.null(subtype)) {
    # ensure it's a string, then split on commas
    countryList <- trimws(
      unlist(strsplit(as.character(subtype), split = ",", fixed = TRUE))
    )
    estimates <- estimates %>% filter(Location %in% countryList)
    mediumVariant <- mediumVariant %>% filter(Location %in% countryList)
  }
  # select only the columns we need, and rename
  dfEstimates <- estimates %>%
      select(
      Location,
      Year,
      totalPop = "Total Population, as of 1 January (thousands)",
      growth   = "Population Growth Rate (percentage)"
    )

    dfMediumVariant <- mediumVariant %>%
      select(
      Location,
      Year,
      totalPop = "Total Population, as of 1 January (thousands)",
      growth   = "Population Growth Rate (percentage)"
    )
  # merge the two dataframes
  df <- bind_rows(
  dfEstimates %>%
    mutate(
      totalPop = as.numeric(totalPop),
      growth   = as.numeric(growth)
    ),
  dfMediumVariant %>%
    mutate(
      totalPop = as.numeric(totalPop),
      growth   = as.numeric(growth)
    )
) %>%
  arrange(Year)
  
  x <- as.magpie(
  x        = df,
  spatial  = 1,    # Location
  temporal = 2,    # Year
  data     = 3     # data columns
  )
  list(x = x,
      weight = NULL,
      description = c(type = "UN prospects medium variant",
                      filename = inputFile,
                      unit = "People, %"))
}
