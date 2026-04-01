#' readIEACrudeOilPrice
#'
#' Read in Crude Oil energy prices from International Energy Agency per sector, period, country.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEACrudeOilPrice")
#' }
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr filter select
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_wider_delim separate
#'
readIEACrudeOilPrice <- function(subtype = "WORLD") {
  fStartHorizon <- readEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
    )["fStartHorizon"]
    
    data <- read.table("CRD_TYP_.TXT", header = FALSE) %>%
      filter(
        V4 == "A",
        V3 >= fStartHorizon
      ) %>% select(-c(V4,V6)) %>%
      rename(
        subregion = V1,
        region = V2,
        period = V3,
        unit = V5,
        value = V7
      )
  
  x <- data %>%
    as.quitte() %>%
    as.magpie()
  
  if (subtype == "WORLD") {
    x <- x["WORLD",,"TOTAL"]
    # 159L*(USD_BBL)*7.33=7.33toe
    # 0.915 dollars 2020 to 2015
    # thousand dollars / 1000
    x <- x * 7.33 * 0.915 / 1000
    getItems(x,3.1) <- "kUSD_toe"
    x <- collapseDim(x, 3.2)
  }
  
  list(
    x = x,
    weight = NULL,
    description = c(
      category = "Prices",
      type = "Energy Crude Oil prices",
      filename = "CRD_CTY_.TXT",
      `Indicative size (MB)` = 7.5,
      dimensions = "3D",
      unit = "various",
      Confidential = "E3M"
    )
  )
}
