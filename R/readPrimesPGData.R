#' readPrimesPGData
#'
#' Read Primes Secondary Energy Electricity Gross and Gross Electricity
#' generation by fuel type for EU countries:
#' 
#' @param subtype excel sheet "Summary" for SEElec or "power generation" for
#' Gross Electricity generation by fuel type
#' 
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- readSource("PrimesPGData", subtype = "Summary")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter %>% select
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_wider pivot_longer

readPrimesPGData <- function(subtype) {
  
  if (subtype == "Summary") {
    files <- list.files(".")
    sheet <- subtype
    
    x <- NULL
    for (i in files) {
      x1 <- helper3(i, sheet, files)
      x <- mbind(x, x1)
    }
  }
  
  if (subtype == "power generation") {
    files <- list.files(".")
    sheet <- subtype
    
    x <- NULL
    for (i in files) {
      x1 <- helper4(i, sheet, files)
      x <- mbind(x, x1)
    }
  }
  
  list(
    x = x,
    weight = NULL,
    description = c(
      category = "Secondary Energy Electricity Gross and Gross Electricity generation by fuel type",
      type = "Secondary Energy Electricity Gross and Gross Electricity generation by fuel type",
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
  x1 <- x1[c(1,33:46),c(1:16)]
  
  names(x1) <- x1[1, ]
  names(x1)[1] <- "variable"
  x1 <- x1[-c(1,2,4,10), ]
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
                                            "EL" = "GRC"
                                          )
  )
  x1["scenario"] <- substr(files[1], 9, 21)
  x1 <- as.quitte(x1)
  x1[["unit"]] <- "GWh"
  x1[["variable"]] <- "Secondary Energy|Electricity"
  x1 <- filter(x1, !is.na(x1[["value"]]))
  x1 <- as.quitte(x1)
  x1 <- as.magpie(x1)
  return(x1)
}


# Helper ------------------------------------------------------------------------------------
helper4 <- function(excel_name, ex_sheet, files) {
  x1 <- read_excel(excel_name, sheet = ex_sheet)
  x1 <- x1[c(1,33:46),c(1:16)]
  
  names(x1) <- x1[1, ]
  names(x1)[1] <- "variable"
  x1 <- x1[-c(1,2,4,10), ]
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
                                            "EL" = "GRC"
                                          )
  )
  x1["scenario"] <- substr(files[1], 9, 21)
  x1 <- as.quitte(x1)
  x1[["unit"]] <- "GWh"
  x1 <- filter(x1, !is.na(x1[["value"]]))
  x1 <- as.quitte(x1)
  x1 <- as.magpie(x1)
  return(x1)
}
