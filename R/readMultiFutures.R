#' readMultiFutures
#'
#' Read in data from the EXIOMOD project.
#' The dataset contains activities data.
#'
#' @return The read-in activities data into a magpie object
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("MultiFutures")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom stringr str_remove
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr %>% across mutate
#' @importFrom readxl read_excel
#' @importFrom stringi stri_escape_unicode
#'
readMultiFutures <- function(subtype = "activities") {
  
  if (subtype == "activities") {
    
    activities <- read_excel("EXIOMOD_to_OPENPROM_GreenGrowth.xlsx",
                             sheet = "perc_diff_output_scen_wrt_ref")
    
    activities <- select(activities, - "...1")
    
    names(activities)[1] <- "region"
    
    names(activities)[2] <- "variable"
    
    activities <- activities %>%
      pivot_longer(
        cols = -c(region, variable),
        names_to = "period",
        values_to = "value"
      ) %>%
      mutate(period = as.integer(period))
    
    activities <- as.quitte(activities) %>% as.magpie()
    
    region_OPEN_PROM_EXIOMOD <- read.csv("region_OPEN-PROM_to_model.txt", header = FALSE)
    
    region_OPEN_PROM_EXIOMOD <- region_OPEN_PROM_EXIOMOD %>%
      separate(col = 1, into = c("regionOP", "regionEXIOMOD"), sep = "\\.")
    
    region_OPEN_PROM_EXIOMOD <- region_OPEN_PROM_EXIOMOD %>%
      mutate(
        regionEXIOMOD = if_else(regionOP == "CHA", "CHN", regionEXIOMOD)
      )
    
    activities <- toolAggregate(
      activities,
      weight = NULL,
      dim = 1,
      rel = region_OPEN_PROM_EXIOMOD,
      from = "regionEXIOMOD",
      to = "regionOP"
    )
    
    mappingACTV <- data.frame(
      OP = c("AG", "IS", "NF", "CH", "OI", "FD", "SE", "HOU", "EN", "BM", "PP"),
      EX = c("iAGR", "iIAS", "iNFM", "iCHE", "iOIS", "iFDT", "iSER", "HH", "iCON", "'iNMM", "iPP")
    )
    
    activities <- toolAggregate(
      activities[,,mappingACTV[,"EX"]],
      weight = NULL,
      dim = 3,
      rel = mappingACTV,
      from = "EX",
      to = "OP"
    )
    
    x <- activities
    
  }
  
  
  if (subtype == "GDP") {
    
    GDP <- read_excel("EXIOMOD_to_OPENPROM_GreenGrowth.xlsx",
                             sheet = "GDP_in_MEUR")
    
    GDP <- GDP[1:10,]
    
    GDP <- select(GDP, - "...1")
    
    names(GDP)[1] <- "region"
    
    GDP <- GDP %>%
      pivot_longer(
        cols = -c(region),
        names_to = "period",
        values_to = "value"
      ) %>%
      mutate(period = as.integer(period))
    
    growth <- as.quitte(GDP) %>%
      arrange(region, period) %>% # Sort by region, and period
      group_by(region) %>% # Group by region
      mutate(
        prev_value = lag(value),
        diff_ratio = value / if_else(prev_value == 0, 1, prev_value)
      ) %>%
      ungroup()
    
    growth <- select(growth, c("region", "variable", "unit", "period", "diff_ratio"))
    names(growth) <- sub("diff_ratio", "value", names(growth))
    growth[["vairable"]] <- "growth rates GDP_in_MEUR"
    
    GDPgrowth <- as.quitte(growth) %>% as.magpie()
    
    region_OPEN_PROM_EXIOMOD <- read.csv("region_OPEN-PROM_to_model.txt", header = FALSE)
    
    region_OPEN_PROM_EXIOMOD <- region_OPEN_PROM_EXIOMOD %>%
      separate(col = 1, into = c("regionOP", "regionEXIOMOD"), sep = "\\.")
    
    region_OPEN_PROM_EXIOMOD <- region_OPEN_PROM_EXIOMOD %>%
      mutate(
        regionEXIOMOD = if_else(regionOP == "CHA", "CHN", regionEXIOMOD)
      )
    
    GDPgrowth <- toolAggregate(
      GDPgrowth,
      weight = NULL,
      dim = 1,
      rel = region_OPEN_PROM_EXIOMOD,
      from = "regionEXIOMOD",
      to = "regionOP"
    )
    
    x <- GDPgrowth
    x <- x[,setdiff(getYears(x), c("y2019","y2020","y2021","y2022","y2023")),]
    
  }
  
  list(x = x,
       weight = NULL,
       description = c(category = "EXIOMOD ACTIVITIES",
                       type = "ACTIVITIES",
                       filename = "engage-EXIOMOD_to_OPENPROM_GreenGrowth.xlsx",
                       `Indicative size (MB)` = 0.784,
                       dimensions = "4D",
                       unit = "variOus",
                       Confidential = "project"))
  
}