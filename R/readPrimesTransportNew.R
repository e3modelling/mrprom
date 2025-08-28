#' readPrimesTransportNew
#'
#' Read Primes data:
#' 
#' @param subtype excel sheet (FuelOutlook, Stock or Indicators)
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- readSource("PrimesTransportNew", subtype = "FuelOutlook")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter %>% select rowwise tibble
#' @importFrom readxl read_excel
#' @importFrom tidyr unnest_wider

readPrimesTransportNew <- function(subtype = "FuelOutlook") {
  
  files <- list.files(".")
  
  
  if (subtype == "FuelOutlook") {
    mapping <- subtype
    x <- NULL
    for (i in files) {
      x1 <- lapply(mapping, function(sheet) {
        x1 <- readSheet2(i, sheet, mapping, files)
        return(x1)
      })
      x <- mbind(x, do.call(mbind, x1))
    }
  }
  
  if (subtype == "Stock") {
    mapping <- subtype
    x <- NULL
    for (i in files) {
      x1 <- lapply(mapping, function(sheet) {
        x1 <- readSheet3(i, sheet, mapping, files)
        return(x1)
      })
      x <- mbind(x, do.call(mbind, x1))
    }
  }
  
  if (subtype == "Indicators") {
    mapping <- subtype
    x <- NULL
    for (i in files) {
      x1 <- lapply(mapping, function(sheet) {
        x1 <- readSheet4(i, sheet, mapping, files)
        return(x1)
      })
      x <- mbind(x, do.call(mbind, x1))
    }
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
readSheet2 <- function(excel_name, ex_sheet, map, files) {
  
  df <- read_excel(excel_name, sheet = ex_sheet)
  
  df <- df[,-c(2,17:23)]
  
  names(df) <- df[1, ]
  names(df)[1] <- "variable"
  df <- df[-c(1:2,1403), ]
  
  search_start <- "Liquid Fuels"
  search_end <- "Total Hydrogen"
  
  # Find all start and end indices
  starts <- which(df$variable == search_start)
  ends <- which(df$variable == search_end)
  
  # Extract all blocks
  blocks_df <- bind_rows(lapply(seq_along(starts), function(i) {
    df[starts[i]:ends[i], ] %>% mutate(Block_ID = i)
  }))
  
  map_of_blocks <- data.frame(sector = c("PC","PT","PA","PB","PN","GU","GT","GN", "PC"),  
                              value = c(9, 22, 27, 4, 34, 14, 26, 35, 13),  stringsAsFactors = FALSE ) 
  
  blocks_df <- blocks_df %>%
    left_join(map_of_blocks, by = c("Block_ID" = "value"))
  
  tr <- data.frame(path = c("Liquid Fuels",
                            "Liquid Fuels|Gasoline blend",
                            "Liquid Fuels|Gasoline blend|Gasoline",
                            "Liquid Fuels|Gasoline blend|Bio Gasoline",
                            "Liquid Fuels|Gasoline blend|Synthetic (P2X) Gasoline",
                            "Liquid Fuels|Ethanol",
                            "Liquid Fuels|Diesel blend",
                            "Liquid Fuels|Diesel blend|Diesel",
                            "Liquid Fuels|Diesel blend|Bio Diesel",
                            "Liquid Fuels|Diesel blend|Synthetic (P2X) Diesel",
                            "Liquid Fuels|DME",
                            "Liquid Fuels|B100",
                            "Liquid Fuels|Jet Fuel",
                            "Liquid Fuels|Jet Fuel|Kerosene",
                            "Liquid Fuels|Jet Fuel|Bio Kerosene",
                            "Liquid Fuels|Jet Fuel|Synthetic (P2X) Kerosene",
                            "Liquid Fuels|Residual fuel oil blend",
                            "Liquid Fuels|Residual fuel oil blend|Residual fuel oil",
                            "Liquid Fuels|Residual fuel oil blend|Bio Heavy",
                            "Gaseous Fuels",
                            "Gaseous Fuels|Natural Gas with H2 blend",
                            "Gaseous Fuels|Natural Gas with H2 blend|Natural Gas",
                            "Gaseous Fuels|Natural Gas with H2 blend|Hydrogen Blended",
                            "Gaseous Fuels|Natural Gas with H2 blend|Clean Gas",
                            "Gaseous Fuels|Natural Gas with Biogas blend",
                            "Gaseous Fuels|Natural Gas with Biogas blend|Natural Gas",
                            "Gaseous Fuels|Natural Gas with Biogas blend|Biogas",
                            "Gaseous Fuels|Natural Gas with Biogas blend|Clean Gas",
                            "Gaseous Fuels|Methane from Biogas",
                            "Gaseous Fuels|Liquefied Petroleum Gas",
                            "Hydrogen for Fuel Cell Vehicles",
                            "Electricity",
                            "Total Oil products",
                            "Total Natural Gas",
                            "Total Biomass",
                            "Total Liquid Synthetic",
                            "Total Gaseous Synthetic (Clean Gas)",
                            "Total Hydrogen"),  
                   value = (blocks_df %>% filter(Block_ID %in% map_of_blocks[["value"]])),  stringsAsFactors = FALSE ) 

  tr <- select(tr, -c(value.variable, value.Block_ID))
  names(tr)[1] <- "variable"
  names(tr) <- gsub("value.", "", names(tr))
  
  x <- tr %>% pivot_longer(!c("sector","variable"), names_to = "period", values_to = "value")
  
  #add light duty vehicles to PC
  x <- x %>%
    group_by(variable, sector, period) %>%
    mutate(value = sum(value)) %>%
    distinct()
  
  max_levels <- max(sapply(strsplit(x[["variable"]], "\\|"), length))
  
  # Split and pad categories
  x <- x %>%
    mutate(variable = strsplit(as.character(variable), "\\|")) %>%
    rowwise() %>%
    mutate(variable = list(c(variable, rep(tail(variable, 1), max_levels - length(variable))))) %>%
    unnest_wider(variable, names_sep = "")
  
  # Rename columns for clarity
  names(x) <- c("Category", "Type", "Fuel", "sector", "period", "value")
  
  x <- as.quitte(x)
  
  x["region"] <- substr(excel_name, 1, 2)
  if (excel_name == "EU28_TRANSPORT_REF2020_v3.xlsx") {
    x["region"] <- "EU28"
  } else if (excel_name == "EU27_TRANSPORT_REF2020_v3.xlsx") {
    x["region"] <- "EU27"
  } else if (excel_name == "EU12_TRANSPORT_REF2020_v3.xlsx") {
    x["region"] <- "EU12"
  } else if (excel_name == "EU15_TRANSPORT_REF2020_v3.xlsx") {
    x["region"] <- "EU15"
  } else if (excel_name == "EU27noUK_TRANSPORT_REF2020_v3.xlsx") {
    x["region"] <- "EU27noUK"
  }
  
  x[["region"]] <- toolCountry2isocode(x[["region"]],
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
  x["scenario"] <- substr(files[1], 14, 20)
  x <- as.quitte(x)
  x[["unit"]] <- "ktoe"
  x <- filter(x, !is.na(x[["region"]]))
  x <- as.quitte(x)
  x <- as.magpie(x)
  return(x)
}


# Helper ------------------------------------------------------------------------------------
readSheet3 <- function(excel_name, ex_sheet, map, files) {
  
  df <- read_excel(excel_name, sheet = ex_sheet)
  
  df <- df[,-c(2:4,19:25)]
  
  names(df) <- df[1, ]
  names(df)[1] <- "variable"
  df <- df[-c(1:2,99), ]
  
  PA <- df[which(df[1] == "AVIATION"),]
  
  search_start <- c("Diesel Conventional", "Diesel")
  search_end <- c("Hydrogen", "Hydrgen") # multiple valid end values
  
  # Find all start and end indices
  starts <- which(df$variable %in% search_start)
  ends_all <- which(df$variable %in% search_end)
  
  # Extract all blocks safely
  blocks_list <- lapply(seq_along(starts), function(i) {
    # Find first end after current start
    end_i <- ends_all[ends_all > starts[i]][1]
    
    # Only extract if a valid end exists
    if (!is.na(end_i)) {
      df[starts[i]:end_i, ] %>% mutate(Block_ID = i)
    } else {
      NULL
    }
  })
  
  # Combine results
  blocks_df <- bind_rows(blocks_list)
  
  map_of_blocks <- data.frame(sector = c("PB","PC","PC","GU","PT","GT","PN","GN"), ### missing,"PA"
                              value = c(1, 3, 5, 4, 6, 7, 8, 9),  stringsAsFactors = FALSE )
  
  blocks_df <- blocks_df %>%
    left_join(map_of_blocks, by = c("Block_ID" = "value"))
  
  paths <- c("Diesel Conventional",
             "Diesel Hybrid",
             "Diesel plug-in hybrid",
             "Gasoline Conventional",
             "Gasoline Hybrid",
             "Gasoline plug-in hybrid",
             "LPG",
             "CNG",
             "E85",
             "Electric",
             "Hydrogen")
  
  tr <- tibble(path = paths) %>%
    left_join(blocks_df %>% filter(Block_ID %in% map_of_blocks[["value"]]), 
              by = c("path" = "variable"))
  
  tr <- select(tr, -c(Block_ID))
  
  #add PA
  PA <-cbind(PA,"PA")
  names(PA) <- sub("\"PA\"","sector", names(PA))
  names(tr) <- sub("path","variable", names(tr))
  
  tr <- rbind(tr, PA)
  tr <- drop_na(tr)
  
  x <- tr %>% pivot_longer(!c("sector","variable"), names_to = "period", values_to = "value")
  
  #add light duty vehicles to PC
  x <- x %>%
    group_by(variable, sector, period) %>%
    summarise(value = sum(value), .groups = "drop")
  
  x <- as.quitte(x)
  
  x["region"] <- substr(excel_name, 1, 2)
  if (excel_name == "EU28_TRANSPORT_REF2020_v3.xlsx") {
    x["region"] <- "EU28"
  } else if (excel_name == "EU27_TRANSPORT_REF2020_v3.xlsx") {
    x["region"] <- "EU27"
  } else if (excel_name == "EU12_TRANSPORT_REF2020_v3.xlsx") {
    x["region"] <- "EU12"
  } else if (excel_name == "EU15_TRANSPORT_REF2020_v3.xlsx") {
    x["region"] <- "EU15"
  } else if (excel_name == "EU27noUK_TRANSPORT_REF2020_v3.xlsx") {
    x["region"] <- "EU27noUK"
  }
  
  x[["region"]] <- toolCountry2isocode(x[["region"]],
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
  x["scenario"] <- substr(files[1], 14, 20)
  x <- as.quitte(x)
  x[["unit"]] <- "ktoe"
  x <- x %>% filter(!is.na(region))
  x <- as.quitte(x)
  x <- as.magpie(x)
  return(x)
}



# Helper ------------------------------------------------------------------------------------
readSheet4 <- function(excel_name, ex_sheet, map, files) {
  
  df <- read_excel(excel_name, sheet = ex_sheet)

  names(df) <- df[2, ]
  names(df)[1] <- "variable"
  
  df <- df[-c(1:4), -c(2:4,19:25)]
  df <- df[c(1:16),]
  
  df[["sector"]] <- "PC"
  
  df[1,1] <- "Internal combustion engine"
  df[2,1] <- "Internal combustion engine|LPG"
  df[3,1] <- "Internal combustion engine|Gasoline"
  df[4,1] <- "Internal combustion engine|Diesel oil"
  df[5,1] <- "Internal combustion engine|Natural gas"
  df[6,1] <- "Internal combustion engine|Ethanol"
  df[7,1] <- "Conv. Hybrid"
  df[8,1] <- "Conv. Hybrid|Gasoline"
  df[9,1] <- "Conv. Hybrid|Diesel oil"
  df[10,1] <- "Plug In Hybrid"
  df[11,1] <- "Plug In Hybrid|Gasoline"
  df[12,1] <- "Plug In Hybrid|Diesel oil"
  df[13,1] <- "Plug In Hybrid|Electricity"
  df[14,1] <- "Pure Electric Vehicles"
  df[15,1] <- "Fuel cells and other"
  df[16,1] <- "Fuel cells and other|Hydrogen"
  
  df <- df %>% pivot_longer(!c("sector","variable"), names_to = "period", values_to = "value")
  
  x <- df
  
  max_levels <- max(sapply(strsplit(x[["variable"]], "\\|"), length))
  
  # Split and pad categories
  x <- x %>%
    mutate(variable = strsplit(as.character(variable), "\\|")) %>%
    rowwise() %>%
    mutate(variable = list(c(variable, rep(tail(variable, 1), max_levels - length(variable))))) %>%
    unnest_wider(variable, names_sep = "")
  
  # Rename columns for clarity
  names(x) <- c("Category", "Fuel", "sector", "period", "value")
  
  x <- as.quitte(x)
  
  x["region"] <- substr(excel_name, 1, 2)
  if (excel_name == "EU28_TRANSPORT_REF2020_v3.xlsx") {
    x["region"] <- "EU28"
  } else if (excel_name == "EU27_TRANSPORT_REF2020_v3.xlsx") {
    x["region"] <- "EU27"
  } else if (excel_name == "EU12_TRANSPORT_REF2020_v3.xlsx") {
    x["region"] <- "EU12"
  } else if (excel_name == "EU15_TRANSPORT_REF2020_v3.xlsx") {
    x["region"] <- "EU15"
  } else if (excel_name == "EU27noUK_TRANSPORT_REF2020_v3.xlsx") {
    x["region"] <- "EU27noUK"
  }
  
  x[["region"]] <- toolCountry2isocode(x[["region"]],
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
  x["scenario"] <- substr(files[1], 14, 20)
  x <- as.quitte(x)
  x[["unit"]] <- "ktoe"
  x <- x %>% filter(!is.na(region))
  x <- as.quitte(x)
  x <- as.magpie(x)
  return(x)
}
