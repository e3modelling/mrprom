#' readPRIMESCCSData
#'
#' Read PRIMES Gross Electricity Generation by CCS plant type for EU countries
#' from the "detCCS" sheet of the V<XX>REF2020upd_v1detinfo.xlsx files.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Christos Koumparakis
#'
#' @examples
#' \dontrun{
#' a <- readSource("PRIMESCCSData")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter %>%
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer

readPRIMESCCSData <- function() {

  files <- list.files(".", pattern = "\\.xlsx$")

  x <- NULL
  for (i in files) {
    x1 <- helperCCS(i, files)
    x <- mbind(x, x1)
  }

  list(
    x = x,
    weight = NULL,
    description = c(
      category = "PRIMES Gross Electricity Generation by CCS plant type",
      type = "PRIMES Gross Electricity Generation by CCS plant type",
      filename = "V<XX>REF2020upd_v1detinfo.xlsx",
      `Indicative size (MB)` = 13,
      dimensions = "4D",
      unit = "TWh",
      Confidential = "E3M"
    )
  )
}

# Helper -----------------------------------------------------------------------
helperCCS <- function(excel_name, files) {
  # detCCS sheet: year header in Excel row 3, "Net Power Generation in CCS plants
  # (GWh) by fuel" block in Excel rows 14-18 (Coal, Lignite, Gaseous Fuels,
  # Liquid Fuels, Biomass), columns 1:16 (label + 15 year columns 2000-2070).
  x1 <- read_excel(excel_name, sheet = "detCCS", col_names = FALSE)
  x1 <- x1[c(3, 14:18), c(1:16)]

  names(x1) <- x1[1, ]
  names(x1)[1] <- "variable"
  x1 <- x1[-1, ]
  x1 <- x1 %>% pivot_longer(!"variable", names_to = "period", values_to = "value")

  # Filenames look like V<XX>REF2020upd_v1detinfo.xlsx; pull the <XX> bit.
  region_code <- sub("REF.*$", "", sub("^V", "", excel_name))
  x1["region"] <- region_code

  suppressWarnings({
    x1[["region"]] <- toolCountry2isocode(x1[["region"]],
                                          mapping =
                                            c(
                                              "EU28" = "EU28",
                                              "EU27" = "EU27",
                                              "EU12" = "EU12",
                                              "EU15" = "EU15",
                                              "EU27noUK" = "EU27noUK",
                                              "EL" = "GRC"))
  })

  scenario_str <- regmatches(files[1], regexpr("REF[0-9]+upd_v[0-9]+", files[1]))
  if (length(scenario_str) == 0) scenario_str <- "REF2020upd_v1"
  x1["scenario"] <- scenario_str

  x1 <- as.quitte(x1)
  x1[["unit"]] <- "TWh"
  x1 <- filter(x1, !is.na(x1[["value"]]))
  x1 <- as.quitte(x1)
  x1 <- as.magpie(x1)
  # GWh to TWh
  x1 <- x1 / 1000
  return(x1)
}
