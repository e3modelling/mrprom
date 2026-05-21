#' readPrimesFuelPrices
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("PrimesFuelPrices")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr mutate if_else if_all all_of filter
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_wider pivot_longer fill
#' @importFrom tibble tibble

readPrimesFuelPrices <- function() {
  
  files <- list.files(".")
  mapping <- list(
    openprom = c(
      "PrimesFuelPrices"
    ),
    primes = c(
      "detfuelpri"
    )
  )
  
  files <- files[!(files %in% c(
    "VEU27REF2020upd_v1detinfo.xlsx","VEU28REF2020upd_v1detinfo.xlsx"
  ))]
  
  x <- NULL
  for (i in files) {
    print(paste("Reading file:", i))
    x1 <- lapply(mapping$primes, function(sheet) {
      x1 <- readSheet(i, sheet, mapping, files)
      return(x1)
    })
    x <- mbind(x, do.call(mbind, x1))
  }
  
  list(
    x = x,
    weight = NULL,
    description = c(
      category = "PrimesFuelPrices",
      type = "PrimesFuelPrices",
      filename = "VATREF2020upd_v1detinfo.xlsx",
      `Indicative size (MB)` = 30,
      dimensions = "4D",
      unit = "various",
      Confidential = "E3M"
    )
  )
}

# Helper ------------------------------------------------------------------------------------
readSheet <- function(excel_name, ex_sheet, map, files) {
  x1 <- read_excel(excel_name, sheet = ex_sheet)
  x1 <- x1[c(3,4,10:17,34,35,105:109,119:122,129,130), c(1, 3:16)]
  names(x1) <- x1[1, ]
  names(x1)[1] <- "subsector"
  x1 <- x1[-1, ]
  
  year_cols <- names(x1)[-1]
  
  x1 <- x1 %>%
    mutate(
      fuel = if_else(
        if_all(all_of(year_cols), is.na),
        subsector,
        NA_character_
      )
    ) %>%
    fill(fuel)
  
  x1 <- x1 %>%
    filter(!if_all(all_of(year_cols), is.na))
  
  x1 <- x1 %>% pivot_longer(!c("fuel","subsector"), names_to = "period", values_to = "value")
  
  
  x1["variable"] <- map$openprom[which(map$primes == ex_sheet)]
  x1["region"] <- substr(excel_name, 2, 3)
  
  suppressWarnings({
    x1[["region"]] <- toolCountry2isocode(x1[["region"]],
                                          mapping =
                                            c(
                                              "EU28" = "EU28",
                                              "EU27" = "EU27",
                                              "EU12" = "EU12",
                                              "EU15" = "EU15",
                                              "EU27noUK" = "EU27noUK",
                                              "EL" = "GRC"
                                            )
    )
  })
  
  x1["scenario"] <- substr(files[1], 4, 10)
  x1 <- as.quitte(x1)
  x1[["unit"]] <- "PRE TAX PRICE (in €/toe)"
  
  x1 <- filter(x1, !is.na(x1[["region"]]))
  x1 <- as.quitte(x1)
  x1 <- as.magpie(x1)
  
  mapping_fuels <- tibble(
    fuel = c(
      "Diesel oil",
      "Gasoline",
      "Kerosene",
      "Biodiesel",
      "Biogasoline",
      "Biokerosene"),
    code = c(
      "GDO",
      "GSL",
      "KRS",
      "BGDO",
      "BGSL",
      "BKRS"
    )
  )
  
  x1 <- toolAggregate(x1, dim = 3.5, rel = mapping_fuels, from = "fuel", to = "code")
  
  return(x1)
}
