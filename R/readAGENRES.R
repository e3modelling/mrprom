#' readAGENRES
#'
#' Read AGENRES data energy use per activity.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("AGENRES")
#' }
#'
#' @importFrom dplyr left_join select
#' @importFrom tidyr pivot_longer fill
#' @importFrom readxl read_excel
#' @importFrom tibble tibble
#' @importFrom stringr str_detect str_extract
#'
readAGENRES <- function() {
  
  x <- read_excel("WP1 Dataset.xlsx", sheet = "Animals - energy summary")
  
  x <- x[,2:27]
  
  names(x)[1] <- "region"
  
  # Extract metadata from first two rows (excluding region)
  groups <- as.character(unlist(x[1, -1]))
  groups[groups == "NA"] <- NA
  groups <- fill(tibble(type = groups), type)$type
  
  activities <- as.character(unlist(x[2, -1]))
  
  col_info <- tibble(
    name = names(x)[-1],
    variable = groups,
    type = activities
  )
  
  # Remove header rows, pivot to long format, and attach metadata
  x <- x[-c(1, 2), ] %>%
    pivot_longer(
      cols = -region,
      names_to = "name",
      values_to = "value"
    ) %>%
    left_join(col_info, by = "name") %>%
    select(region, type, variable, value)
  
  x["unit"] <- "TJ"
  x[["value"]] <- as.numeric(x[["value"]])
  x <- as.quitte(x)
  
  levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]), mapping =
                                                 c("World" = "GLO"))
  
  x <- as.magpie(x)
  
  x2 <- read_excel("WP1 Dataset.xlsx", sheet = "Diesel total use for crops")
  x2 <- x2[-c(1,2),-c(1,10)]
  names(x2)[1] <- c("region")
  
  x2 <- x2 %>%
    pivot_longer(
      cols = -region,
      names_to = "type",
      values_to = "value"
    ) 
  
  x2[["variable"]] <- "Crops"
  x2[["unit"]] <- "TJ"
  
  x2 <- x2 %>%
    mutate(region = str_extract(region, "(?<=\\()[^()]+(?=\\))"))
  
  x2[["value"]] <- as.numeric(x2[["value"]])
  
  x2 <- as.quitte(x2)
  
  levels(x2[["region"]]) <- toolCountry2isocode(levels(x2[["region"]]), mapping =
                                                 c("World" = "GLO",
                                                   "EL" = "GRC"))
  x2 <- as.magpie(x2)
  
  x3 <- read_excel("WP1 Dataset.xlsx", sheet = "Greenhouses")
  
  x3 <- x3[-c(2,3),c(1,9,10)]
  names(x3) <- x3[1,]
  
  x3 <- x3[!is.na(x3[[1]]), ]
  names(x3)[1] <- "region"
  names(x3)[2] <- "Rest"
  
  x3 <- filter(x3, region != "Total")
  
  x3 <- x3 %>%
    mutate(
      Rest = as.numeric(Rest) - as.numeric(Heating),
      Heating = as.numeric(Heating)
    )
  
  x3 <- x3 %>%
    pivot_longer(
      cols = -region,
      names_to = "type",
      values_to = "value"
    ) 
  
  x3[["variable"]] <- "Greenhouses"
  x3[["unit"]] <- "TJ"
  
  x3[["value"]] <- as.numeric(x3[["value"]])
  
  x3 <- as.quitte(x3)
  
  levels(x3[["region"]]) <- toolCountry2isocode(levels(x3[["region"]]), mapping =
                                                  c("World" = "GLO",
                                                    "EL" = "GRC"))
  x3 <- as.magpie(x3)
  
  x3 <- add_columns(x3 , addnm = "GBR", dim = 1, fill = 0)
  
  final <- mbind(x, x2, x3)
  
  # TJ to Mtoe
  final <- final / 41868
  getItems(final, 3.2) <- "Mtoe"
  
  x4 <- read_excel("WP1 Dataset.xlsx", sheet = "Greenhouses")
  
  x4 <- x4[-c(2,3),c(1,3)]
  
  names(x4)[1] <- "region"
  x4 <- x4[!is.na(x4[[1]]), ]
  x4 <- filter(x4, region != "Total")
  
  x4[["variable"]] <- "Greenhouses"
  x4[["type"]] <- "The area under high covers"
  x4[["unit"]] <- "ha"
  names(x4) <- sub("The area under high covers", "value", names(x4))
  
  x4[["value"]] <- as.numeric(x4[["value"]])
  
  x4 <- as.quitte(x4)
  
  levels(x4[["region"]]) <- toolCountry2isocode(levels(x4[["region"]]), mapping =
                                                  c("World" = "GLO",
                                                    "EL" = "GRC"))
  x4 <- as.magpie(x4)
  
  x4 <- add_columns(x4 , addnm = "GBR", dim = 1, fill = 0)
  
  x5 <- read_excel("WP1 Dataset.xlsx", sheet = "Scale of animal production")
  units <- x5[1,-1]
  x5 <- x5[2:26,-1]
  
  names(x5)[1] <- "region"
  names(units)[1] <- "region"
  
  # Extract units for each variable
  col_info <- units %>%
    pivot_longer(
      cols = -region,
      names_to = "variable",
      values_to = "unit"
    ) %>%
    select(variable, unit)
  
  # Convert data to long format and attach units
  x5 <- x5 %>%
    pivot_longer(
      cols = -region,
      names_to = "variable",
      values_to = "value"
    ) %>%
    left_join(col_info, by = "variable") %>%
    mutate(
      type = "Scale of animal production",
      value = as.numeric(value),
      period = NA
    ) %>%
    select(region, period, variable, type, unit, value)
  
  # Convert to quitte
  x5 <- as.quitte(x5)
  
  # Convert country names to ISO codes
  levels(x5[["region"]]) <- toolCountry2isocode(
    levels(x5[["region"]]),
    mapping = c("World" = "GLO")
  )
  
  # Convert to magpie
  x5 <- as.magpie(x5)
  
  x6 <- read_excel("WP1 Dataset.xlsx", sheet = "Animals - energy summary")
  
  x6 <- x6[, c(2, 29:33)]
  
  names(x6)[1] <- "region"
  
  # Extract metadata from first two rows
  variables <- as.character(unlist(x6[1, -1]))
  
  col_info <- tibble(
    name = names(x6)[-1],
    variable = variables,
    type = "TOTAL ENERGY USE"
  )
  
  # Remove header rows, pivot to long format, and attach metadata
  x6 <- x6[-c(1, 2), ] %>%
    pivot_longer(
      cols = -region,
      names_to = "name",
      values_to = "value"
    ) %>%
    left_join(col_info, by = "name") %>%
    select(region, type, variable, value)
  
  x6[["unit"]] <- "TJ"
  x6[["value"]] <- as.numeric(x6[["value"]])
  
  x6 <- as.quitte(x6)
  
  levels(x6[["region"]]) <- toolCountry2isocode(
    levels(x6[["region"]]),
    mapping = c("World" = "GLO")
  )
  
  x6 <- as.magpie(x6)
  
  final <- mbind(x, x2, x3, x4, x5, x6)
  
  
  list(x = final,
       weight = NULL,
       description = c(category = "AGENRES",
                       type = "energy use per activity",
                       filename = "WP1 Dataset.xlsx",
                       `Indicative size (MB)` = 8,
                       dimensions = "3D",
                       unit = "Mtoe",
                       Confidential = "E3M"))
}
