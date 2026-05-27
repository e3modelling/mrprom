#' readIEA_EV
#'
#' Read IEA_EV data from IEA.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_EV", convert = TRUE)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter select
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readIEA_EV <- function() {
  x <- read_excel("EVDataExplorer2025.xlsx",
    sheet = "GEVO_EV_2025"
  )

  x <- select(x, -"Aggregate group")

  names(x) <- sub("region_country", "region", names(x))
  names(x) <- sub("year", "period", names(x))
  names(x) <- sub("mode", "variable", names(x))

  x <- as.quitte(x)
  x <- drop_na(x)
  
  suppressWarnings({
    x[["region"]] <- toolCountry2isocode((x[["region"]]), mapping =
                                           c("Dem. Rep. of Congo" = "COD",
                                             "DPR of Korea" = "PRK",
                                             "Islamic Rep. of Iran" = "IRN",
                                             "Kingdom of Eswatini" = "SWZ",
                                             "People's Rep. of China" = "CHN",
                                             "Republic of Turkiye" = "TUR",
                                             "United Rep. of Tanzaniae" = "TZA",
                                             "Europe" = "EUR",
                                             "Rest of the world" = "RWRL"))
  })
  
  x <- filter(x, !is.na(x[["region"]]))
  x <- as.quitte(x)
  x <- unique(x)
  x <- as.magpie(x)

  list(
    x = x,
    weight = NULL,
    description = c(
      category = "Vehicles stock",
      type = "Vehicles stock",
      filename = "EVDataExplorer2025.xlsx",
      `Indicative size (MB)` = 0.91,
      dimensions = "4D",
      unit = "various",
      Confidential = "E3M"
    )
  )
}
