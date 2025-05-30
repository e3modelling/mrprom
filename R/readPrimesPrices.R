#' readPrimesPrices
#'
#' Read Primes Prices per fuel and sector for EU countries:
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- readSource("PrimesPrices")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter %>% select
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_wider pivot_longer

readPrimesPrices <- function() {
  files <- list.files(".")
  
  sheet <- "detfuelpri"
  
  x <- NULL
  for (i in files) {
    x1 <- lapply(sheet, function(sheet) {
      x1 <- prices_helper(i, sheet, files)
      return(x1)
    })
    x <- mbind(x, do.call(mbind, x1))
  }
  
  list(
    x = x,
    weight = NULL,
    description = c(
      category = "Prices per fuel and sector",
      type = "Prices per fuel and sector",
      filename = "VATREF2020upd_v1detinfo.xlsx",
      `Indicative size (MB)` = 24,
      dimensions = "4D",
      unit = "various",
      Confidential = "E3M"
    )
  )
}

# Helper ------------------------------------------------------------------------------------
prices_helper <- function(excel_name, ex_sheet, files) {
  x <- read_excel(excel_name, sheet = ex_sheet)
  x1 <- x[c(1:154), c(1:16)]
  var <- x1[2, 2]
  
  x1[,"...2"] <- as.numeric(unlist(x1[,"...2"]))
  
  names(x1) <- x1[3, ]

  x1 <- x1[-c(1:3), ]
  names(x1)[1] <- "variable"
  
  x1 <- filter(x1, !is.na(x1[["variable"]]))
  
  x1 <- x1 %>% pivot_longer(!"variable", names_to = "period", values_to = "value")
  
  x1["fuel"] <- NA
  x1[1 : 150, 4] <- "Diesel oil"
  x1[151 : 210, 4] <- "Gasoline"
  x1[211 : 255, 4] <- "Fuel oil"
  x1[256 : 360, 4] <- "LPG"
  x1[361 : 390, 4] <- "Byproducts"
  x1[391 : 420, 4] <- "Naptha"
  x1[421 : 450, 4] <- "Other liquid fuels"
  x1[451 : 480, 4] <- "Kerosene"
  x1[481 : 615, 4] <- "Natural gas"
  x1[616 : 720, 4] <- "Solids"
  x1[721 : 780, 4] <- "Biomass"
  x1[781 : 810, 4] <- "Waste"
  x1[811 : 870, 4] <- "Methanol"
  x1[871 : 915, 4] <- "Ethanol"
  x1[916 : 975, 4] <- "H2F"
  x1[976 : 1110, 4] <- "Electricity"
  x1[1111 : 1200, 4] <- "Steam"
  x1[1201 : 1320, 4] <- "Hydrogen"
  x1[1321 : 1395, 4] <- "Fossil Diesel"
  x1[1396 : 1470, 4] <- "Biodiesel"
  x1[1471 : 1545, 4] <- "Synthetic Diesel"
  x1[1546 : 1605, 4] <- "Fossil Gasoline"
  x1[1606 : 1665, 4] <- "Biogasoline"
  x1[1666 : 1725, 4] <- "Synthetic Gasoline"
  x1[1726 : 1755, 4] <- "Fossil Kerosene"
  x1[1756 : 1785, 4] <- "Biokerosene"
  x1[1786 : 1815, 4] <- "Synthetic Kerosene"
  x1[1816 : 1845, 4] <- "Fossil Fuel Oil"
  x1[1846 : 1875, 4] <- "Bioheavy"
  x1[1876 : 1905, 4] <- "Synthetic Fuel Oil"
  x1[1906 : 1965, 4] <- "Fossil Natural gas"
  x1[1966 : 2025, 4] <- "Biogas"
  x1[2026 : 2085, 4] <- "Hydrogen Blended"
  x1[2086 : 2145, 4] <- "Clean Gas"
  
  x1 <- x1[x1[,"variable"] != x1[,"fuel"], ]
  
  x1 <- as.quitte(x1)
  
  x1[["unit"]] <- "€/toe"
  
  x1[["sector"]] <- as.character(var)
  
  sector1 <- x1
  ##################
  
  x1 <- x[c(1:154), c(1,17:31)]
  var <- x1[2, 2]
  
  x1[,"...17"] <- as.numeric(unlist(x1[,"...17"]))
  
  names(x1) <- x1[3, ]
  
  x1 <- x1[-c(1:3), ]
  names(x1)[1] <- "variable"
  
  x1 <- filter(x1, !is.na(x1[["variable"]]))
  
  x1 <- x1 %>% pivot_longer(!"variable", names_to = "period", values_to = "value")
  
  x1["fuel"] <- NA
  x1[1 : 150, 4] <- "Diesel oil"
  x1[151 : 210, 4] <- "Gasoline"
  x1[211 : 255, 4] <- "Fuel oil"
  x1[256 : 360, 4] <- "LPG"
  x1[361 : 390, 4] <- "Byproducts"
  x1[391 : 420, 4] <- "Naptha"
  x1[421 : 450, 4] <- "Other liquid fuels"
  x1[451 : 480, 4] <- "Kerosene"
  x1[481 : 615, 4] <- "Natural gas"
  x1[616 : 720, 4] <- "Solids"
  x1[721 : 780, 4] <- "Biomass"
  x1[781 : 810, 4] <- "Waste"
  x1[811 : 870, 4] <- "Methanol"
  x1[871 : 915, 4] <- "Ethanol"
  x1[916 : 975, 4] <- "H2F"
  x1[976 : 1110, 4] <- "Electricity"
  x1[1111 : 1200, 4] <- "Steam"
  x1[1201 : 1320, 4] <- "Hydrogen"
  x1[1321 : 1395, 4] <- "Fossil Diesel"
  x1[1396 : 1470, 4] <- "Biodiesel"
  x1[1471 : 1545, 4] <- "Synthetic Diesel"
  x1[1546 : 1605, 4] <- "Fossil Gasoline"
  x1[1606 : 1665, 4] <- "Biogasoline"
  x1[1666 : 1725, 4] <- "Synthetic Gasoline"
  x1[1726 : 1755, 4] <- "Fossil Kerosene"
  x1[1756 : 1785, 4] <- "Biokerosene"
  x1[1786 : 1815, 4] <- "Synthetic Kerosene"
  x1[1816 : 1845, 4] <- "Fossil Fuel Oil"
  x1[1846 : 1875, 4] <- "Bioheavy"
  x1[1876 : 1905, 4] <- "Synthetic Fuel Oil"
  x1[1906 : 1965, 4] <- "Fossil Natural gas"
  x1[1966 : 2025, 4] <- "Biogas"
  x1[2026 : 2085, 4] <- "Hydrogen Blended"
  x1[2086 : 2145, 4] <- "Clean Gas"
  
  x1 <- x1[x1[,"variable"] != x1[,"fuel"], ]
  
  x1 <- as.quitte(x1)
  
  x1[["unit"]] <- "€/toe"
  
  x1[["sector"]] <- as.character(var)
  
  sector2 <- x1
  ##################
  ##################
  
  x1 <- x[c(1:154), c(1,62:76)]
  var <- x1[2, 2]
  
  x1[,"...62"] <- as.numeric(unlist(x1[,"...62"]))
  
  names(x1) <- x1[3, ]
  
  x1 <- x1[-c(1:3), ]
  names(x1)[1] <- "variable"
  
  x1 <- filter(x1, !is.na(x1[["variable"]]))
  
  x1 <- x1 %>% pivot_longer(!"variable", names_to = "period", values_to = "value")
  
  x1["fuel"] <- NA
  x1[1 : 150, 4] <- "Diesel oil"
  x1[151 : 210, 4] <- "Gasoline"
  x1[211 : 255, 4] <- "Fuel oil"
  x1[256 : 360, 4] <- "LPG"
  x1[361 : 390, 4] <- "Byproducts"
  x1[391 : 420, 4] <- "Naptha"
  x1[421 : 450, 4] <- "Other liquid fuels"
  x1[451 : 480, 4] <- "Kerosene"
  x1[481 : 615, 4] <- "Natural gas"
  x1[616 : 720, 4] <- "Solids"
  x1[721 : 780, 4] <- "Biomass"
  x1[781 : 810, 4] <- "Waste"
  x1[811 : 870, 4] <- "Methanol"
  x1[871 : 915, 4] <- "Ethanol"
  x1[916 : 975, 4] <- "H2F"
  x1[976 : 1110, 4] <- "Electricity"
  x1[1111 : 1200, 4] <- "Steam"
  x1[1201 : 1320, 4] <- "Hydrogen"
  x1[1321 : 1395, 4] <- "Fossil Diesel"
  x1[1396 : 1470, 4] <- "Biodiesel"
  x1[1471 : 1545, 4] <- "Synthetic Diesel"
  x1[1546 : 1605, 4] <- "Fossil Gasoline"
  x1[1606 : 1665, 4] <- "Biogasoline"
  x1[1666 : 1725, 4] <- "Synthetic Gasoline"
  x1[1726 : 1755, 4] <- "Fossil Kerosene"
  x1[1756 : 1785, 4] <- "Biokerosene"
  x1[1786 : 1815, 4] <- "Synthetic Kerosene"
  x1[1816 : 1845, 4] <- "Fossil Fuel Oil"
  x1[1846 : 1875, 4] <- "Bioheavy"
  x1[1876 : 1905, 4] <- "Synthetic Fuel Oil"
  x1[1906 : 1965, 4] <- "Fossil Natural gas"
  x1[1966 : 2025, 4] <- "Biogas"
  x1[2026 : 2085, 4] <- "Hydrogen Blended"
  x1[2086 : 2145, 4] <- "Clean Gas"
  
  x1 <- x1[x1[,"variable"] != x1[,"fuel"], ]
  
  x1 <- as.quitte(x1)
  
  x1[["unit"]] <- "%"
  
  x1[["sector"]] <- as.character(var)
  
  sector3 <- x1
  ##################
  ##################
  
  x1 <- x[c(1:154), c(1,77:91)]
  var <- x1[2, 2]
  
  x1[,"...77"] <- as.numeric(unlist(x1[,"...77"]))
  
  names(x1) <- x1[3, ]
  
  x1 <- x1[-c(1:3), ]
  names(x1)[1] <- "variable"
  
  x1 <- filter(x1, !is.na(x1[["variable"]]))
  
  x1 <- x1 %>% pivot_longer(!"variable", names_to = "period", values_to = "value")
  
  x1["fuel"] <- NA
  x1[1 : 150, 4] <- "Diesel oil"
  x1[151 : 210, 4] <- "Gasoline"
  x1[211 : 255, 4] <- "Fuel oil"
  x1[256 : 360, 4] <- "LPG"
  x1[361 : 390, 4] <- "Byproducts"
  x1[391 : 420, 4] <- "Naptha"
  x1[421 : 450, 4] <- "Other liquid fuels"
  x1[451 : 480, 4] <- "Kerosene"
  x1[481 : 615, 4] <- "Natural gas"
  x1[616 : 720, 4] <- "Solids"
  x1[721 : 780, 4] <- "Biomass"
  x1[781 : 810, 4] <- "Waste"
  x1[811 : 870, 4] <- "Methanol"
  x1[871 : 915, 4] <- "Ethanol"
  x1[916 : 975, 4] <- "H2F"
  x1[976 : 1110, 4] <- "Electricity"
  x1[1111 : 1200, 4] <- "Steam"
  x1[1201 : 1320, 4] <- "Hydrogen"
  x1[1321 : 1395, 4] <- "Fossil Diesel"
  x1[1396 : 1470, 4] <- "Biodiesel"
  x1[1471 : 1545, 4] <- "Synthetic Diesel"
  x1[1546 : 1605, 4] <- "Fossil Gasoline"
  x1[1606 : 1665, 4] <- "Biogasoline"
  x1[1666 : 1725, 4] <- "Synthetic Gasoline"
  x1[1726 : 1755, 4] <- "Fossil Kerosene"
  x1[1756 : 1785, 4] <- "Biokerosene"
  x1[1786 : 1815, 4] <- "Synthetic Kerosene"
  x1[1816 : 1845, 4] <- "Fossil Fuel Oil"
  x1[1846 : 1875, 4] <- "Bioheavy"
  x1[1876 : 1905, 4] <- "Synthetic Fuel Oil"
  x1[1906 : 1965, 4] <- "Fossil Natural gas"
  x1[1966 : 2025, 4] <- "Biogas"
  x1[2026 : 2085, 4] <- "Hydrogen Blended"
  x1[2086 : 2145, 4] <- "Clean Gas"
  
  x1 <- x1[x1[,"variable"] != x1[,"fuel"], ]
  
  x1 <- as.quitte(x1)
  
  x1[["unit"]] <- "€/toe"
  
  x1[["sector"]] <- as.character(var)
  
  sector4 <- x1
  ##################
  

  df <- rbind(sector1,sector2,sector3,sector4)

  df["region"] <- substr(excel_name, 2, 3)
  if (excel_name == "VEU28REF2020upd_v1detinfo.xlsx") {
    df["region"] <- "EU28"
  } else if (excel_name == "VEU27REF2020upd_v1detinfo.xlsx") {
    df["region"] <- "EU27"
  }
  
  df[["region"]] <- toolCountry2isocode(df[["region"]],
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
  df["scenario"] <- substr(files[1], 4, 23)
  df <- as.quitte(df)
  df <- filter(df, !is.na(df[["region"]]))
  df <- as.quitte(df)
  df <- as.magpie(df)
  return(df)
}
