#' readIEATOTPRICES2026
#'
#' Read in energy prices from International Energy Agency per sector, period, country and fuel.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEATOTPRICES2026")
#' }
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_wider_delim separate
#'
readIEATOTPRICES2026 <- function() {
  if (!file.exists("IEATOTPRICES2026.rds")) {
    fStartHorizon <- toolReadEvalGlobal(
      system.file(file.path("extdata", "main.gms"), package = "mrprom")
    )["fStartHorizon"]
 
    data1 <- read.csv("ENDPRICES_1960_2024.csv", header = FALSE) %>%
      slice(-1) %>%
      select(c("V3", "V5", "V7","V9", "V11", "V13","V15", "V17", "V19")) %>%
      rename(
        region = V3,
        product = V5,
        fuel = V7,
        variable = V9,
        CURENCY = V11,
        unit = V13,
        period = V17,
        value = V19
      ) %>%
      filter(
        period >= fStartHorizon,
        product == "PRICE_TOTAL",
        CURENCY == "USD_R",
        V15 == "A", # Keep annual values
        !grepl("-", region) # Keep regions with no "-" in the name (sub-regions)
      ) %>%
      select(-c("V15", "product")) %>%
      select(c("region", "period", "variable", "unit", "fuel", "CURENCY", "value"))
    
    data2 <- read.csv("ENDPRICES_2025_2026.csv", header = FALSE) %>%
      slice(-1) %>%
      select(c("V3", "V5", "V7","V9", "V11", "V13","V15", "V17", "V19")) %>%
      rename(
        region = V3,
        product = V5,
        fuel = V7,
        variable = V9,
        CURENCY = V11,
        unit = V13,
        period = V17,
        value = V19
      ) %>%
      filter(
        period >= fStartHorizon,
        product == "PRICE_TOTAL",
        CURENCY == "USD_R",
        V15 == "A", # Keep annual values
        !grepl("-", region) # Keep regions with no "-" in the name (sub-regions)
      ) %>%
      select(-c("V15", "product")) %>%
      select(c("region", "period", "variable", "unit", "fuel", "CURENCY", "value"))
    
    data <- rbind(data1, data2)
    
    saveRDS(object = data, file = "IEATOTPRICES2026.rds")
  } else {
    data <- readRDS("IEATOTPRICES2026.rds")
  }
  
  x <- data %>%
    mutate(value = as.numeric(value)) %>%
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
