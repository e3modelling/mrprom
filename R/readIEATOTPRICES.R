#' readIEATOTPRICES
#'
#' Read in energy prices from International Energy Agency per sector, period, country and fuel.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEATOTPRICES")
#' }
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_wider_delim separate
#'
readIEATOTPRICES <- function() {
  if (!file.exists("IEATOTPRICES.rds")) {
    fStartHorizon <- readEvalGlobal(
      system.file(file.path("extdata", "main.gms"), package = "mrprom")
    )["fStartHorizon"]

    data <- read.table("END_PRICES.txt", header = FALSE) %>%
      rename(
        region = V1,
        fuel = V2,
        variable = V3,
        product = V5,
        period = V6,
        CURENCY = V7,
        value = V8,
        # V9 is qualifier value (actual value, imputed, etc.)
        unit = V10
      ) %>%
      filter(
        period >= fStartHorizon,
        product == "PRICE_TOTAL",
        CURENCY == "USD_R",
        V4 == "A", # Keep annual values
        !grepl("-", region) # Keep regions with no "-" in the name (sub-regions)
      ) %>%
      select(-c("V4", "V9", "product"))
    saveRDS(object = data, file = "IEATOTPRICES.rds")
  } else {
    data <- readRDS("IEATOTPRICES.rds")
  }

  x <- data %>%
    as.quitte() %>%
    as.magpie()

  list(
    x = x,
    weight = NULL,
    description = c(
      category = "Costs",
      type = "Energy prices",
      filename = "END_PRICES.txt",
      `Indicative size (MB)` = 5500,
      dimensions = "3D",
      unit = "various",
      Confidential = "E3M"
    )
  )
}
