#' readPrimesPGData
#'
#' Read Primes Secondary Energy Electricity Gross Electricity generation by plant type
#' and Net Installed Power Capacity for EU countries:
#' 
#' @param subtype excel sheet "SE" for SE Electricity or "power generation" for
#' Gross Electricity generation by plant type and "capacity" for
#' Net Installed Power Capacity
#' 
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("PrimesPGData", subtype = "SE")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter %>% select
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_wider pivot_longer

readPrimesPGData <- function(subtype = "SE") {
  
  if (subtype == "SE") {
    files <- list.files(".")
    sheet <- "Summary"
    
    x <- NULL
    for (i in files) {
      x1 <- helper3(i, sheet, files)
      x <- mbind(x, x1)
    }
  }
  
  if (subtype == "power generation") {
    files <- list.files(".")
    sheet <- "pg-detail"
    
    x <- NULL
    for (i in files) {
      x1 <- helper4(i, sheet, files)
      x <- mbind(x, x1)
    }
  }
  
  if (subtype == "capacity") {
    files <- list.files(".")
    sheet <- "pg-detail"
    
    x <- NULL
    for (i in files) {
      x1 <- helper5(i, sheet, files)
      x <- mbind(x, x1)
    }
  }
  
  list(
    x = x,
    weight = NULL,
    description = c(
      category = "Primes Secondary Energy Electricity Gross Electricity generation by plant type
      and Net Installed Power Capacity",
      type = "Primes Secondary Energy Electricity Gross Electricity generation by plant type
      and Net Installed Power Capacity",
      filename = "PRepDet_REF2020upd_v1AT.xlsx",
      `Indicative size (MB)` = 13,
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
  
  x1["region"] <- substr(excel_name, nchar(excel_name) - 6, nchar(excel_name) - 5)
  
  if (excel_name == "PRepDet_REF2020upd_v1EU28.xlsx") {
    x1["region"] <- "EU28"
  } else if (excel_name == "PRepDet_REF2020upd_v1EU27.xlsx") {
    x1["region"] <- "EU27"
  } 
  
  x1[["region"]] <- toolCountry2isocode(x1[["region"]],
                                        mapping =
                                          c("EU28" = "EU28",
                                            "EU27" = "EU27",
                                            "EU12" = "EU12",
                                            "EU15" = "EU15",
                                            "EU27noUK" = "EU27noUK",
                                            "EL" = "GRC"))
  
  x1["scenario"] <- substr(files[1], 9, 21)
  x1 <- as.quitte(x1)
  x1[["unit"]] <- "TWh"
  x1[["variable"]] <- "Secondary Energy|Electricity"
  x1 <- filter(x1, !is.na(x1[["value"]]))
  x1 <- as.quitte(x1)
  x1 <- as.magpie(x1)
  #GWh to TWh
  x1 <- x1 / 1000
  return(x1)
}

# Helper ------------------------------------------------------------------------------------
helper4 <- function(excel_name, ex_sheet, files) {
  x1 <- read_excel(excel_name, sheet = ex_sheet)
  x1 <- x1[c(1,48,51,52,54,55,56,59,60,61,64,65,66),c(1:16)]
  
  names(x1) <- x1[1, ]
  names(x1)[1] <- "variable"
  x1 <- x1[-c(1), ]
  x1 <- x1 %>% pivot_longer(!"variable", names_to = "period", values_to = "value")
  
  
  x1["region"] <- substr(excel_name, nchar(excel_name) - 6, nchar(excel_name) - 5)
  
  if (excel_name == "PRepDet_REF2020upd_v1EU28.xlsx") {
    x1["region"] <- "EU28"
  } else if (excel_name == "PRepDet_REF2020upd_v1EU27.xlsx") {
    x1["region"] <- "EU27"
  } 
  
  x1[["region"]] <- toolCountry2isocode(x1[["region"]],
                                        mapping =
                                          c(
                                            "EU28" = "EU28",
                                            "EU27" = "EU27",
                                            "EU12" = "EU12",
                                            "EU15" = "EU15",
                                            "EU27noUK" = "EU27noUK",
                                            "EL" = "GRC"))
  
  x1["scenario"] <- substr(files[1], 9, 21)
  x1 <- as.quitte(x1)
  x1[["unit"]] <- "TWh"
  x1 <- filter(x1, !is.na(x1[["value"]]))
  x1 <- as.quitte(x1)
  x1 <- as.magpie(x1)
  #GWh to TWh
  x1 <- x1 / 1000
  return(x1)
}

# Helper ------------------------------------------------------------------------------------
helper5 <- function(excel_name, ex_sheet, files) {
  x1 <- read_excel(excel_name, sheet = ex_sheet)
  x1 <- x1[c(1,6,9,10,12,13,14,17,18,19,22,23,24),c(1:16)]
  
  names(x1) <- x1[1, ]
  names(x1)[1] <- "variable"
  x1 <- x1[-c(1), ]
  x1 <- x1 %>% pivot_longer(!"variable", names_to = "period", values_to = "value")
  
  
  x1["region"] <- substr(excel_name, nchar(excel_name) - 6, nchar(excel_name) - 5)
  
  if (excel_name == "PRepDet_REF2020upd_v1EU28.xlsx") {
    x1["region"] <- "EU28"
  } else if (excel_name == "PRepDet_REF2020upd_v1EU27.xlsx") {
    x1["region"] <- "EU27"
  } 
  
  x1[["region"]] <- toolCountry2isocode(x1[["region"]],
                                        mapping =
                                          c(
                                            "EU28" = "EU28",
                                            "EU27" = "EU27",
                                            "EU12" = "EU12",
                                            "EU15" = "EU15",
                                            "EU27noUK" = "EU27noUK",
                                            "EL" = "GRC"))
  
  x1["scenario"] <- substr(files[1], 9, 21)
  x1 <- as.quitte(x1)
  x1[["unit"]] <- "GW"
  x1 <- filter(x1, !is.na(x1[["value"]]))
  x1 <- as.quitte(x1)
  x1 <- as.magpie(x1)
  #MW to GW
  x1 <- x1 / 1000
  return(x1)
}
