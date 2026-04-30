#' readPrimesBalances
#'
#' Read Primes data Final Energy Demand in DOMSE, INDSE, NENSE, TRANSE sector:
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
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
  setwd("C:/Users/sioutas/Ricardo Plc/Global Integrated Assessment Models - Documents/Work/PROMETHEUS Model/madratverse/sources/PrimesBalances")
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
  
  df_growth <- q %>%
    filter(fuel == "Total") %>%
    arrange(region, variable, unit, period) %>%
    group_by(region, variable, unit) %>%
    mutate(
      growth_rate = lead(value) / value
    ) %>%
    ungroup()
  
  df_wide <- df_growth %>%
    select(model, scenario, region, variable, unit, fuel, period, growth_rate) %>%
    pivot_wider(
      names_from = period,
      values_from = growth_rate
    )
  
  df_share <- q %>%
    group_by(scenario, region, variable, unit, period) %>%
    mutate(
      total_value = first(value[fuel == "Total"]),
      share_fuel = ifelse(!is.na(total_value), value / total_value, NA_real_)
    ) %>%
    ungroup()
  
  df_wide_share <- df_share %>%
    select(model, scenario, region, variable, unit, fuel, period, share_fuel) %>%
    pivot_wider(
      names_from = period,
      values_from = share_fuel
    )
  
  q[["model"]] <- "PrimesBalances"
  
  x <- calcOutput(type = "IFuelCons2", aggregate = FALSE)[unique(q[["region"]]),,]
  x <- x[,c(2010,2015,2020),]
  total_OP <- dimSums(x, dim = 3.2)
  total_OP <- add_dimension(total_OP, dim = 3.2, add = "ef", nm = "Total")
  x <- mbind(x, total_OP)
  x <- x[,,intersect(getItems(x,3.2),unique(q[["fuel"]]))]
  xq <- as.quitte(x) %>%
    select(c("period", "value", "region", "dsbs", "ef")) %>% rename(variable = dsbs, fuel = ef)
  xq[["model"]] <- "OP"
  xq[["scenario"]] <- "Historical"
  xq[["unit"]] <- "Mtoe"
  
  df_rbind <- bind_rows(xq, q)
  
  df_rbind_wide <- df_rbind %>%
    pivot_wider(
      names_from = period,
      values_from = value
    )
  
  library(openxlsx)
  
  write.xlsx(df_rbind_wide, "two_models.xlsx")
  
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
    "blast furnace gas" = "Other Gases",
    "hydro" = "Hydro",
    "gasoline" = "Gasoline",
    "kerosene" = "Kerosene",
    "other liquids" = "Other Liquids",
    "patent fuels" = "Biodiesel",
    "other solids" = "Hard Coal, Coke and Other Solids",
    "natural gas incl_ clean gas" = "Natural Gas",
    "hydrogen (incl_ distributed and directly used)" = "Hydrogen",
    "steam" = "Steam",
    "liqufied petroleum gas" = "Liquefied Petroleum Gas",
    "gasworks gas" = "Other Gases",
    "biomass-waste" = "Biomass and Waste",
    "nuclear" = "Nuclear",
    "Crude oil" = "Crude Oil and Feedstocks",
    "coke-oven gas" = "Other Gases",
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
      "Biodiesel", "Hydrogen",
      "Electricity"
    ),
    code = c(
      "Total", "HCL", "LGN", "CRO", "LPG", "GSL", "KRS", "GDO", "RFO",
      "OLQ", "NGS", "OGS", "NUC", "STE", "HYD", "WND", "SOL", "BMSWAS",
      "GEO", "MET", "ETH", "BGDO", "H2F", "ELC"
    )
  )
  
  x1 <- toolAggregate(x1, dim = 3.4, rel = mapping_fuels_df, from = "fuel", to = "code")
  
  return(x1)
}
