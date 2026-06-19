#' readIRENA
#'
#' Read IRENA
#' The dataset contains Capacity and generation data.
#'
#' @return The read-in carbon price data into a magpie object
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IRENA")
#' }
#'
#' @importFrom dplyr filter mutate %>%
#' @importFrom stringr str_extract str_trim str_remove
#' @importFrom quitte as.quitte
#'
readIRENA <- function() {
 
  ####### generation on grid
  generationOnGrid <- read.csv(
    "Electricity generation by IRENA on grid.csv",
    sep = ",",
    skip = 1,          # skip the title row
    stringsAsFactors = FALSE
  )
  
  names(generationOnGrid) <- gsub("Country.area", "region", names(generationOnGrid))
  names(generationOnGrid) <- gsub("Technology", "variable", names(generationOnGrid))
  names(generationOnGrid) <- gsub("Data.Type", "type", names(generationOnGrid))
  names(generationOnGrid) <- gsub("Grid.connection", "Grid", names(generationOnGrid))
  names(generationOnGrid) <- gsub("Year", "period", names(generationOnGrid))
  names(generationOnGrid) <- gsub("Electricity.generation.statistics", "value", names(generationOnGrid))
  
  generationOnGrid <- generationOnGrid %>%
    mutate(
      unit = str_extract(type, "(?<=\\().+(?=\\))"),
      type = str_trim(str_remove(type, "\\s*\\(.*\\)"))
    )
  
  generationOnGrid <- generationOnGrid %>%
    filter(value != "-") %>%          # remove non-numeric values
    mutate(value = as.numeric(value)) 

  ####### generation off grid
  generationOffGrid <- read.csv(
    "Electricity generation by IRENA off grid.csv",
    sep = ",",
    skip = 1,          # skip the title row
    stringsAsFactors = FALSE
  )
  
  names(generationOffGrid) <- gsub("Country.area", "region", names(generationOffGrid))
  names(generationOffGrid) <- gsub("Technology", "variable", names(generationOffGrid))
  names(generationOffGrid) <- gsub("Data.Type", "type", names(generationOffGrid))
  names(generationOffGrid) <- gsub("Grid.connection", "Grid", names(generationOffGrid))
  names(generationOffGrid) <- gsub("Year", "period", names(generationOffGrid))
  names(generationOffGrid) <- gsub("Electricity.generation.statistics", "value", names(generationOffGrid))
  
  generationOffGrid <- generationOffGrid %>%
    mutate(
      unit = str_extract(type, "(?<=\\().+(?=\\))"),
      type = str_trim(str_remove(type, "\\s*\\(.*\\)"))
    )
  
  generationOffGrid <- generationOffGrid %>%
    filter(value != "-") %>%          # remove non-numeric values
    mutate(value = as.numeric(value)) 
  
  ####### generation all
  generation <- read.csv(
    "Electricity generation by IRENA ALL.csv",
    sep = ",",
    skip = 1,          # skip the title row
    stringsAsFactors = FALSE
  )
  
  names(generation) <- gsub("Country.area", "region", names(generation))
  names(generation) <- gsub("Technology", "variable", names(generation))
  names(generation) <- gsub("Data.Type", "type", names(generation))
  names(generation) <- gsub("Grid.connection", "Grid", names(generation))
  names(generation) <- gsub("Year", "period", names(generation))
  names(generation) <- gsub("Electricity.generation.statistics", "value", names(generation))
  
  generation <- generation %>%
    mutate(
      unit = str_extract(type, "(?<=\\().+(?=\\))"),
      type = str_trim(str_remove(type, "\\s*\\(.*\\)"))
    )
  
  generation <- generation %>%
    filter(value != "-") %>%          # remove non-numeric values
    mutate(value = as.numeric(value)) 
  
  ####### HEATGEN
  HEATGEN <- read.csv(
    "HEATGEN_20260602-101223.csv",
    sep = ",",
    skip = 1,          # skip the title row
    stringsAsFactors = FALSE
  )
  
  names(HEATGEN) <- gsub("Country.area", "region", names(HEATGEN))
  names(HEATGEN) <- gsub("Technology", "variable", names(HEATGEN))
  names(HEATGEN) <- gsub("Grid.connection", "Grid", names(HEATGEN))
  names(HEATGEN) <- gsub("Year", "period", names(HEATGEN))
  names(HEATGEN) <- gsub("Heat.generation..TJ.", "value", names(HEATGEN))
  
  HEATGEN[["type"]] <- "Heat generation"
  HEATGEN[["unit"]] <- "TJ"
  
  HEATGEN <- HEATGEN %>%
    filter(value != "-") %>%          # remove non-numeric values
    mutate(value = as.numeric(value)) 
  
  ####### Electricity capacity statistics by IRENA on grid
  CapacityOnGrid <- read.csv(
    "Electricity capacity statistics by IRENA on grid.csv",
    sep = ",",
    skip = 1,          # skip the title row
    stringsAsFactors = FALSE
  )
  
  names(CapacityOnGrid) <- gsub("Country.area", "region", names(CapacityOnGrid))
  names(CapacityOnGrid) <- gsub("Technology", "variable", names(CapacityOnGrid))
  names(CapacityOnGrid) <- gsub("Grid.connection", "Grid", names(CapacityOnGrid))
  names(CapacityOnGrid) <- gsub("Year", "period", names(CapacityOnGrid))
  names(CapacityOnGrid) <- gsub("Electricity.capacity.statistics", "value", names(CapacityOnGrid))
  
  CapacityOnGrid[["type"]] <- "Electricity capacity on grid"
  CapacityOnGrid[["unit"]] <- "MW"
  
  CapacityOnGrid <- CapacityOnGrid %>%
    filter(value != "-") %>%          # remove non-numeric values
    mutate(value = as.numeric(value)) 
  
  ####### Electricity capacity statistics by IRENA off grid
  CapacityOffGrid <- read.csv(
    "Electricity capacity statistics by IRENA off grid.csv",
    sep = ",",
    skip = 1,          # skip the title row
    stringsAsFactors = FALSE
  )
  
  names(CapacityOffGrid) <- gsub("Country.area", "region", names(CapacityOffGrid))
  names(CapacityOffGrid) <- gsub("Technology", "variable", names(CapacityOffGrid))
  names(CapacityOffGrid) <- gsub("Grid.connection", "Grid", names(CapacityOffGrid))
  names(CapacityOffGrid) <- gsub("Year", "period", names(CapacityOffGrid))
  names(CapacityOffGrid) <- gsub("Electricity.capacity.statistics", "value", names(CapacityOffGrid))
  
  CapacityOffGrid[["type"]] <- "Electricity capacity off grid"
  CapacityOffGrid[["unit"]] <- "MW"
  
  CapacityOffGrid <- CapacityOffGrid %>%
    filter(value != "-") %>%          # remove non-numeric values
    mutate(value = as.numeric(value)) 
  
  x <- rbind(generation, HEATGEN, CapacityOnGrid, CapacityOffGrid, generationOnGrid, generationOffGrid)
  
  x$region <- iconv(
    x$region,
    from = "latin1",
    to = "UTF-8",
    sub = ""
  )
  
  x$region <- gsub(" \\(the\\)", "", x$region)
  
  x[["region"]] <- toolCountry2isocode(x[["region"]], mapping =
                                         c("China, Hong Kong Special Administrative Region" = "HKG",
                                           "Netherlands (Kingdom of the)" = "NLD",
                                           "South Georgia" = "SGS",
                                           "State of Palestine" = "PSE"))
  
  x <- x[!is.na(x$region), ]
  
  x <- as.quitte(x) %>% as.magpie()
  
  list(x = x,
       weight = NULL,
       description = c(category = "Capacity, generation",
                       type = "Capacity, generation",
                       filename = "HEATGEN_20260602-101223.csv",
                       `Indicative size (MB)` = 15,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "project"))
  
}