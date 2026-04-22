#' readIEA_ENERGY_AI_World
#'
#' ReadENERGY and AI data from IEA for World.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_ENERGY_AI_World", convert = TRUE)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter select
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#' @importFrom stringr str_extract str_remove
#'
readIEA_ENERGY_AI_World <- function() {
  
  # Read first sheet
  World_Data <- read_excel("Data_annex_Energy_and_AI.xlsx",
                   sheet = "World Data", skip = 2)
  
  
  names(World_Data) <- gsub("\\*", "", names(World_Data))   # remove *
  names(World_Data) <- sub("\\.\\.\\..*", "", names(World_Data))  # remove ... suffix
  
  names(World_Data)[1] <- "Sector"
  names(World_Data)[c(6,7)] <- paste0(names(World_Data)[c(6,7)], "Base")
  names(World_Data)[c(9,10)] <- paste0(names(World_Data)[c(9,10)], "Lift-Off")
  names(World_Data)[c(12,13)] <- paste0(names(World_Data)[c(12,13)], "High Efficiency")
  names(World_Data)[c(15,16)] <- paste0(names(World_Data)[c(15,16)], "Headwinds")
  World_Data <- World_Data[, names(World_Data) != ""]
  
  x1 <-  World_Data %>% 
    pivot_longer(
      cols = -Sector,      # pivot all columns EXCEPT Category
      names_to = "period",
      values_to = "value"
    )
  
  x1[["variable"]] <- NA
  x1[["unit"]] <- NA
  x1[["subsector"]] <- NA
  x1[1:99,"variable"] <- "Installed capacity"
  x1[1:99,"unit"] <- "GW"
  x1[1:55,"subsector"] <- "Total"
  x1[56:99,"subsector"] <- "IT"
  x1[100:154,"variable"] <- "Power usage effectiveness"
  x1[100:154,"unit"] <- "effectiveness"
  x1[100:154,"subsector"] <- "Total"
  x1[155:209,"variable"] <- "Load factor"
  x1[155:209,"unit"] <- "%"
  x1[155:209,"subsector"] <- "Total"
  x1[210:nrow(x1),"variable"] <- "Electricity consumption"
  x1[210:nrow(x1),"unit"] <- "TWh"
  x1[210:264,"subsector"] <- "Total"
  x1[265:nrow(x1),"subsector"] <- "IT"
  
  x1 <- x1 %>% filter(x1[["value"]] != "NA")
  x1[["regions"]] <- "World"
  names(x2) <- c("region","2020","2023","2024","2030")
  
  x1 <- x1 %>%
    mutate(
      period = as.character(period),
      Year = str_extract(period, "^\\d{4}"),
      Type = str_remove(period, "^\\d{4}"),
      Type = ifelse(Type == "", "Historical", Type)
    ) %>% select(-period) %>% rename(period = Year)
  
  x <- as.quitte(x1) %>% as.magpie()
  
  
  list(
    x = x,
    weight = NULL,
    description = c(
      category = "Energy_and_AI",
      type = "Energy_and_AI",
      filename = "Data_annex_Energy_and_AI.xlsx",
      `Indicative size (MB)` = 0.31,
      dimensions = "3D",
      unit = "various",
      Confidential = "E3M"
    )
  )
}
