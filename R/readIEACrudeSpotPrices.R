#' readIEACrudeSpotPrices
#'
#' Read in Crude Spot Prices from International Energy Agency.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEACrudeSpotPrices")
#' }
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_wider_delim separate
#'
readIEACrudeSpotPrices <- function() {
  if (!file.exists("IEACrudeSpotPrices.rds")) {
    fStartHorizon <- readEvalGlobal(
      system.file(file.path("extdata", "main.gms"), package = "mrprom")
    )["fStartHorizon"]
    
    data <- read.table("Crude_oil_spot_prices.TXT", header = FALSE) %>%
      rename(
        region = V1,
        variable = V2,
        unit = V3,
        period = V4,
        value = V6
      ) %>%
      filter(
        period >= fStartHorizon,
        V5 == "A" # Keep annual values
      ) %>%
      select(-c("V5","V7"))
    saveRDS(object = data, file = "IEACrudeSpotPrices.rds")
  } else {
    data <- readRDS("IEACrudeSpotPrices.rds")
  }
  
  x <- data %>%
    as.quitte() %>%
    as.magpie()
  
  list(
    x = x,
    weight = NULL,
    description = c(
      category = "Costs",
      type = "Crude Spot Prices",
      filename = "Crude_oil_spot_prices.TXT",
      `Indicative size (MB)` = 1.22,
      dimensions = "3D",
      unit = "various",
      Confidential = "E3M"
    )
  )
}
