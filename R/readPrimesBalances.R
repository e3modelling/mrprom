#' readPrimesBalances
#'
#' Read Primes data Final Energy Demand in DOMSE, INDSE, NENSE, TRANSE sector:
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- readSource("PrimesBalances")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter %>% select
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_wider pivot_longer

readPrimesBalances <- function() {
  files <- list.files(".")
  mapping <- list(
    openprom = c(
      "IS", "NF", "PCH", "CH", "OI", "PP", "FD", "TX", "EN", "NEN",
      "PT", "PC", "PA", "PN",
      "HOU", "SE", "AG"
    ),
    primes = c(
      "CIS", "CNF", "CPCH", "CCH", "COTH", "CPP", "CFDT", "CTEX", "CENG", "CFNEN",
      "CTT", "CRT", "CATD", "CNI",
      "CHOU", "CSER", "CAGR"
    )
  )

  x <- NULL
  for (i in files) {
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
      category = "Final energy consumption",
      type = "Final energy consumption",
      filename = "VATREF2020_v3bal.xlsx",
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
  x1 <- x1[c(2:3, 5:39), c(1, 3:18)]
  names(x1) <- x1[1, ]
  names(x1)[1] <- "fuel"
  x1 <- x1[-1, ]
  x1 <- x1 %>% pivot_longer(!"fuel", names_to = "period", values_to = "value")
  x1["variable"] <- map$openprom[which(map$primes == ex_sheet)]
  x1["region"] <- substr(excel_name, 2, 3)
  if (excel_name == "VEU28REF2020upd_v1bal.xlsx") {
    x1["region"] <- "EU28"
  } else if (excel_name == "VEU27REF2020upd_v1bal.xlsx") {
    x1["region"] <- "EU27"
  }
  
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
  x1[["unit"]] <- "ktoe"
  x1 <- filter(x1, !is.na(x1[["region"]]))
  x1 <- as.quitte(x1)
  x1 <- as.magpie(x1)
  return(x1)
}
