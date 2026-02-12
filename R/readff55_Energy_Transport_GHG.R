#' readff55_Energy_Transport_GHG
#'
#' Read energy-transport-ghg of EU
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("ff55_Energy_Transport_GHG", convert = FALSE)
#' }
#' 
#' @importFrom utils read.csv
#' @importFrom dplyr filter select distinct bind_rows mutate
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#' @importFrom readxl read_excel excel_sheets
#'
readff55_Energy_Transport_GHG <- function() {
  
  file <- "ff55_reg_energy-transport-ghg.xlsx"
  
  sheet_names <- excel_sheets(file)
  
  # Remove "Cover" sheet
  sheet_names <- sheet_names[sheet_names != "Cover"]
  
  # Extract region codes (AT, BE, BG, …)
  regions <- unique(sub("_[AB]$", "", sheet_names))
  
  # For each region, read sheet_A and sheet_B and rbind them
  result <- lapply(regions, function(region) {
    
    sheets_to_read <- sheet_names[grepl(paste0("^", region, "_[AB]$"), sheet_names)]
    
    # Read both sheets
    dfs <- lapply(sheets_to_read, function(s) {
      read_excel(path = file, sheet = s)
    })
    
    # Combine
    combined <- bind_rows(dfs, .id = "sheet_id")
    
    return(combined)
  })
  
  names(result) <- regions
  
  x <- NULL
  
  for (i in 1 : length(result)) {
    y <- as.data.frame(result[i])
    y <- y[,-c(1,9,10)]
    
    # Set column names from row 1
    names(y) <- as.numeric(unlist(y[1, ]))
    # Drop row 1
    y <- y[-c(1,8,19,33,35,42,54,56,73,82,92,108,112,118,120:134,153,171,172,207,214,232:242), ]
    y[["region"]] <- regions[[i]]
    
    names(y)[1] <- "variable"
    names(y) <- gsub("^[A-Za-z]+\\.(\\d{4})$", "\\1", names(y))
    y[["sector"]] <- NA
    row.names(y) <- NULL
    # sector
    y[c(1:6),9] <- y[1,1]
    y[c(7:16),9] <- y[7,1]
    y[c(18:29),9] <- y[18,1]
    y[c(30:47),9] <- y[30,1]
    y[c(48),9] <- y[48,1]
    y[c(50:60),9] <- y[50,1]
    y[c(61),9] <- y[61,1]
    y[c(62:64),9] <- y[62,1]
    y[c(65:72),9] <- y[65,1]
    y[c(73:81),9] <- y[73,1]
    y[c(83:88),9] <- y[83,1]
    y[c(89:95),9] <- y[89,1]
    y[c(96),9] <- y[96,1]
    y[c(98:99),9] <- y[98,1]
    y[c(100:102),9] <- y[100,1]
    y[c(103),9] <- y[103,1]
    y[c(104),9] <- y[104,1]
    y[c(105),9] <- y[105,1]
    y[c(107:123),9] <- y[107,1]
    y[c(124:140),9] <- y[124,1]
    y[c(142:148),9] <- y[142,1]
    y[c(149:152),9] <- y[149,1]
    y[c(153:161),9] <- y[153,1]
    y[c(162:164),9] <- y[162,1]
    y[c(165:167),9] <- y[165,1]
    y[c(168:171),9] <- y[168,1]
    y[c(172:174),9] <- y[172,1]
    y[c(176:177),9] <- y[176,1]
    y[c(178:180),9] <- y[178,1]
    y[c(181:187),9] <- y[181,1]
    y[c(188),9] <- y[188,1]
    y[c(189),9] <- y[189,1]
    y[c(190),9] <- y[190,1]
    y[c(191:197),9] <- y[191,1]
    
    y <- y[!is.na(y[["2005"]]), ]
    
    y <- y %>% 
      pivot_longer(
        matches("^\\d{4}$"),    # only 4-digit year columns
        names_to = "period",
        values_to = "value"
      )
    
    y <- as.quitte(y)
    x <- rbind(x, y)
  }
  
  suppressWarnings({
    x[["region"]] <- toolCountry2isocode(x[["region"]],mapping = c("EU" = "EU",
                                                                   "Czech.Republic" = "CZE",
                                                                   "EL" = "GRC",
                                                                   "The.Netherlands" = "NLD"))
  })
  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  x[is.na(x)] <- 0
  
  list(x = x,
       weight = NULL,
       description = c(category = "energy-transport-ghg.xlsx",
                       type = "Emissions",
                       filename = "ff55_reg_energy-transport-ghg.xlsx",
                       `Indicative size (MB)` = 0.850,
                       dimensions = "2D",
                       unit = "energy-transport-ghg",
                       Confidential = "project"))
}
