#' readPrimesBalancesGR
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("PrimesBalancesGR")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter %>% select
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_wider pivot_longer

readPrimesBalancesGR <- function(subtype) {

    files <- list.files(".")
  mapping <- list(
    openprom = c(
      "IS", "NF", "CH", "BM", "OI", "PP", "FD", "TX", "EN",
      "PCH", "NEN",
      "HOU", "SE", "AG"
    ),
    primes = c(
      "CIS", "CNF",  "CCH", "CNMM", "COTH", "CPP", "CFDT", "CTEX", "CENG",
      "CPCH", "CFNEN",
      "CHOU", "CSER", "CAGR"
    )
  )
  
  files <- files[!(files %in% c(
    "VEU27REF2020upd_v1bal.xlsx","VEU28REF2020upd_v1bal.xlsx"
  ))]
  # "OE",is the same with  "COTH" OI
  
  x <- NULL
  for (i in files) {
    print(paste("Reading file:", i))
    x1 <- lapply(mapping$primes, function(sheet) {
      x1 <- readSheet(i, sheet, mapping, files)
      return(x1)
    })
    x <- mbind(x, do.call(mbind, x1))
  }
  
  q <- as.quitte(x)
  
  # if (subtype == "Projections") {
  #   x <- q
  #   x <- filter(x, fuel == "Total") %>%
  #     select(region, variable, period, value) %>%
  #     rename(Sector = variable, Region = region)
  #   
  #   df <- x %>%
  #     mutate(period = as.numeric(period)) %>%
  #     arrange(Region, Sector, period)
  #   
  #   # Compute growth rates and assign them to the lead period
  #   df_growth <- df %>%
  #     group_by(Region, Sector) %>%
  #     arrange(period) %>%
  #     mutate(
  #       growth = (lead(value) / value)^(1 / (lead(period) - period)) - 1,
  #       # growth = (lead(value) / value),
  #       period = lead(period)   # shift period forward
  #     ) %>%
  #     ungroup() %>%
  #     filter(!is.na(growth)) %>%
  #     select(-value) %>%              # 🔑 remove original value
  #     rename(value = growth)
  #   
  #   # Convert back to quitte format and reshape wide
  #   df_growth <- as.quitte(df_growth)
  #   
  #   # df_growth <- df_growth %>%
  #   #   select(region, period, sector, value) %>%
  #   #   rename(variable = sector) %>%
  #   #   pivot_wider(
  #   #     names_from = period,
  #   #     values_from = value
  #   #   )
  #   
  #   x <- df_growth %>%
  #     select(region, period, sector, value) %>%
  #     rename(variable = sector) 
  #   
  #   x <- as.quitte(x)
  #   x <- as.magpie(x)
  # }
  
  if (subtype == "Shares") {
    ###shares
    q_share <- q %>%
      group_by(region, variable, period) %>%
      mutate(total_value = sum(value[fuel == "Total"], na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(share = value / total_value)
    
    q_share <- q_share %>%
      select(region, variable, period, fuel, share) %>%
      rename(value = share) 
    
    # q_share <- as.quitte(q_share) %>%
    #   interpolate_missing_periods(period = 2010:2100, expand.values = TRUE)
    
    # q_share <- q_share %>%
    #   select(region, variable, period, fuel, value) %>%
    #   filter(fuel != "Total") %>%
    #   pivot_wider(
    #     names_from = period,
    #     values_from = value
    #   )
    # 
    x <- q_share %>%
      select(region, variable, period, fuel, value) %>%
      filter(fuel != "Total") 
    
    x <- as.quitte(x)
    x <- as.magpie(x)
  }
  # write_xlsx(q_share, "PrimesShares.xlsx")

  
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
  x1 <- x1[c(2,4:40), c(1, 3:18)]
  names(x1) <- x1[1, ]
  names(x1)[1] <- "fuel"
  x1 <- x1[-1, ]
  
  mappingduelsPrimes = c("Total w/o Ambient Heat1", "hard coal","patent fuels","coke",
                         "tar,pitch,benzol","lignite","other solids","Crude oil",
                         "Feedstocks","refinery gas","liqufied petroleum gas",
                         "gasoline","kerosene","naptha","diesel oil","fuel oil",
                         "other liquids","natural gas incl. clean gas","coke-oven gas",
                         "blast furnace gas","gasworks gas","biomass-waste","nuclear",
                         "hydro","wind","solar","tidal and other renewables",
                         "geothermal heat","methanol","ethanol",
                         "hydrogen (incl. distributed and directly used)","steam",
                         "electricity")
  
  x1 <- x1 %>% filter(fuel %in% mappingduelsPrimes)
  
  
  x1 <- x1 %>% pivot_longer(!"fuel", names_to = "period", values_to = "value")
  
  
  x1["variable"] <- map$openprom[which(map$primes == ex_sheet)]
  x1["region"] <- substr(excel_name, 2, 3)
  
  suppressWarnings({
    x1[["region"]] <- toolCountry2isocode(x1[["region"]],
                                          mapping =
                                            c("EL" = "GRC"
                                            )
    )
  })
  
  x1["scenario"] <- substr(files[1], 4, 10)
  x1 <- as.quitte(x1)
  x1[["unit"]] <- "Mtoe"
  x1[["value"]] <- x1[["value"]] / 1000
  x1 <- filter(x1, !is.na(x1[["region"]]))
  x1 <- as.quitte(x1)
  x1 <- as.magpie(x1)
  
  mappingPRIMESfuelsaggregation <- c(
    "diesel oil" = "Diesel Oil",
    "refinery gas" = "Other Gases",
    "fuel oil" = "Residual Fuel Oil",
    "ethanol" = "Ethanol",
    "Feedstocks" = "Crude Oil and Feedstocks",
    "methanol" = "Methanol",
    "tar,pitch,benzol" = "Hard Coal, Coke and Other Solids",
    "lignite" = "Lignite",
    "wind" = "Wind",
    "electricity" = "Electricity",
    "hard coal" = "Hard Coal, Coke and Other Solids",
    "coke" = "Hard Coal, Coke and Other Solids",
    "blast furnace gas" = "Hard Coal, Coke and Other Solids",
    "hydro" = "Hydro",
    "gasoline" = "Gasoline",
    "kerosene" = "Kerosene",
    "other liquids" = "Other Liquids",
    "patent fuels" = "Hard Coal, Coke and Other Solids",
    "other solids" = "Hard Coal, Coke and Other Solids",
    "natural gas incl_ clean gas" = "Natural Gas",
    "hydrogen (incl_ distributed and directly used)" = "Hydrogen",
    "steam" = "Steam",
    "liqufied petroleum gas" = "Liquefied Petroleum Gas",
    "gasworks gas" = "Other Gases",
    "biomass-waste" = "Biomass and Waste",
    "nuclear" = "Nuclear",
    "Crude oil" = "Crude Oil and Feedstocks",
    "coke-oven gas" = "Hard Coal, Coke and Other Solids",
    "solar" = "Solar",
    "naptha" = "Other Liquids",
    "geothermal heat" = "Geothermal and other renewable sources",
    "Total w/o Ambient Heat1" = "Total",
    "tidal and other renewables" = "Geothermal and other renewable sources"
  )
  mappingPRIMESfuelsaggregation <- data.frame(
    fuel = names(mappingPRIMESfuelsaggregation),
    aggregation = as.vector(mappingPRIMESfuelsaggregation),
    row.names = NULL
  )
  
  x1 <- toolAggregate(x1, dim = 3.4, rel = mappingPRIMESfuelsaggregation, from = "fuel", to = "aggregation")
  
  library(tibble)
  
  mapping_fuels_df <- tibble(
    fuel = c(
      "Total",
      "Hard Coal, Coke and Other Solids",
      "Lignite",
      "Crude Oil and Feedstocks",
      "Liquefied Petroleum Gas",
      "Gasoline",
      "Kerosene",
      "Diesel Oil",
      "Residual Fuel Oil",
      "Other Liquids",
      "Natural Gas",
      "Other Gases",
      "Nuclear",
      "Steam",
      "Hydro",
      "Wind",
      "Solar",
      "Biomass and Waste",
      "Geothermal and other renewable sources",
      "Methanol",
      "Ethanol",
      "Hydrogen",
      "Electricity"
    ),
    code = c(
      "Total", "HCL", "LGN", "CRO", "LPG", "GSL", "KRS", "GDO", "RFO",
      "OLQ", "NGS", "OGS", "NUC", "STE", "HYD", "WND", "SOL", "BMSWAS",
      "GEO", "MET", "ETH", "H2F", "ELC"
    )
  )
  
  x1 <- toolAggregate(x1, dim = 3.4, rel = mapping_fuels_df, from = "fuel", to = "code")
  
  return(x1)
}
