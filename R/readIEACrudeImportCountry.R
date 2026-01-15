#' readIEACrudeImportCountry
#'
#' Read in Crude import per country from International Energy Agency.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEACrudeImportCountry")
#' }
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_wider_delim separate
#'
readIEACrudeImportCountry <- function() {
  if (!file.exists("IEACrudeImportCountry.rds")) {
    fStartHorizon <- readEvalGlobal(
      system.file(file.path("extdata", "main.gms"), package = "mrprom")
    )["fStartHorizon"]
    
    data <- read.table("Crude_import_country.TXT", header = FALSE) %>%
      rename(
        region = V1,
        variable = V2,
        period = V3,
        unit = V5,
        value = V7
      ) %>%
      filter(
        period >= fStartHorizon,
        V4 == "A" # Keep annual values
      ) %>%
      select(-c("V4","V6"))
    saveRDS(object = data, file = "IEACrudeImportCountry.rds")
  } else {
    data <- readRDS("IEACrudeImportCountry.rds")
  }
  
  x <- data %>%
    as.quitte() %>%
    as.magpie()
  
  list(
    x = x,
    weight = NULL,
    description = c(
      category = "Costs",
      type = "Crude import per country",
      filename = "Crude_import_country.TXT",
      `Indicative size (MB)` = 7.4,
      dimensions = "3D",
      unit = "various",
      Confidential = "E3M"
    )
  )
}
