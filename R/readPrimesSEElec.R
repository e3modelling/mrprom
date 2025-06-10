#' readPrimesSEElec
#'
#' Read Primes Prices per fuel and sector for EU countries:
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- readSource("PrimesSEElec")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter %>% select
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_wider pivot_longer

readPrimesSEElec <- function() {
  files <- list.files(".")
  sheet <- "Summary"
  
  x <- NULL
  for (i in files) {
    x1 <- lapply(mapping, function(sheet) {
      x1 <- helper3(i, sheet, files)
      return(x1)
    })
    x <- mbind(x, do.call(mbind, x1))
  }
  list(
    x = x,
    weight = NULL,
    description = c(
      category = "Final energy consumption TRANSPORT",
      type = "Final energy consumption TRANSPORT",
      filename = "AT_TRANSPORT_REF2020_v3.xlsx",
      `Indicative size (MB)` = 30,
      dimensions = "4D",
      unit = "various",
      Confidential = "E3M"
    )
  )
}

# Helper ------------------------------------------------------------------------------------
helper3 <- function(excel_name, ex_sheet, files) {
  x1 <- read_excel(excel_name, sheet = ex_sheet)
  x1 <- x1[c(1, 41),c(1:18)]
  
  names(x1) <- x1[1, ]
  names(x1)[1] <- "variable"
  x1 <- x1[-1, ]
  x1 <- x1 %>% pivot_longer(!"variable", names_to = "period", values_to = "value")
  
  x1["technology"] <- NA
  x1[1 : 42, 4] <- "Road transport"
  x1[43 : 56, 4] <- "Trucks"
  x1[57 : 70, 4] <- "Passenger Light Duty Vehicles"
  x1[71 : 84, 4] <- "Freight Light Duty Vehicles"
  x1[85 : 112, 4] <- "Rail"
  x1[113 : 126, 4] <- "Aviation"
  x1[127 : 140, 4] <- "Inland navigation"
  x1[141 : 434, 4] <- "Public road transport"
  x1[435 : 728, 4] <- "Buses"
  x1[729 : 1022, 4] <- "Private cars"
  x1[1023 : 1316, 4] <- "Passenger Light Duty Vehicles"
  x1[1317 : 1610, 4] <- "Trucks"
  x1[1611 : 1904, 4] <- "Rail"
  x1[1905 : 2198, 4] <- "Passenger trains"
  x1[2199 : 2492, 4] <- "Freight trains"
  x1[2493 : 2786, 4] <- "Aviation"
  x1[2787 : 3080, 4] <- "Inland navigation"
  x1[3081 : 3374, 4] <- "Passenger inland navigation"
  x1[3375 : 3668, 4] <- "Freight inland navigation"
  x1["sector"] <- "Final Energy Demand (in ktoe)"
  
  x1["region"] <- substr(excel_name, 1, 2)
  if (excel_name == "EU28_TRANSPORT_REF2020_v3.xlsx") {
    x1["region"] <- "EU28"
  } else if (excel_name == "EU27_TRANSPORT_REF2020_v3.xlsx") {
    x1["region"] <- "EU27"
  } else if (excel_name == "EU12_TRANSPORT_REF2020_v3.xlsx") {
    x1["region"] <- "EU12"
  } else if (excel_name == "EU15_TRANSPORT_REF2020_v3.xlsx") {
    x1["region"] <- "EU15"
  } else if (excel_name == "EU27noUK_TRANSPORT_REF2020_v3.xlsx") {
    x1["region"] <- "EU27noUK"
  }
  
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
  x1["scenario"] <- substr(files[1], 14, 20)
  x1 <- as.quitte(x1)
  x1[["unit"]] <- "ktoe"
  x1 <- filter(x1, !is.na(x1[["region"]]))
  x1 <- as.quitte(x1)
  x1 <- as.magpie(x1)
  return(x1)
}
