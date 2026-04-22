#' readIEA_ENERGY_AI_Regional
#'
#' ReadENERGY and AI data from IEA Regional.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_ENERGY_AI_Regional", convert = TRUE)
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter select
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#' @importFrom stringr str_extract str_remove
#'
readIEA_ENERGY_AI_Regional <- function() {
  
  # Read second sheet
  Regional_Data <- read_excel("Data_annex_Energy_and_AI.xlsx",
                              sheet = "Regional Data")
  
  x2 <- Regional_Data %>% filter(Regional_Data[["Installed capacity (GW)"]] != "NA")
  
  x2 <- x2 %>% select(-"...5")
  
  x2 <- x2 %>% filter(x2[["...2"]] != "NA")
  
  names(x2) <- c("region","2020","2023","2024","2030")
  
  x2[["variable"]] <- NA
  x2[["unit"]] <- NA
  x2[["subsector"]] <- NA
  x2[1:18,"variable"] <- "Installed capacity"
  x2[1:18,"unit"] <- "GW"
  x2[1:9,"subsector"] <- "Total installed capacity"
  x2[10:18,"subsector"] <- "IT installed capacity"
  
  x2[19:36,"variable"] <- "Power usage effectiveness and load factor"
  x2[19:36,"unit"] <- "%"
  x2[19:27,"subsector"] <- "Power usage effectiveness"
  x2[28:36,"subsector"] <- "Load factor"
  
  x2[37:54,"variable"] <- "Electricity consumption"
  x2[37:54,"unit"] <- "TWh"
  x2[37:45,"subsector"] <- "Total electricity consumption"
  x2[46:54,"subsector"] <- "IT electricity consumption"
  
  # 1. Fix column types FIRST
  x2 <- x2 %>%
    mutate(across(c(`2020`, `2023`, `2024`, `2030`), as.numeric))
  
  x2 <-  x2 %>% 
    pivot_longer(
      cols = -c("region", "variable", "unit", "subsector"),
      names_to = "period",
      values_to = "value"
    )
  
  x <- as.quitte(x2) %>% as.magpie()
  
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
