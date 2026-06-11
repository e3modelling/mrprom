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
  
  final <- mbind(x, x2)
  
  # TJ to Mtoe
  final <- final / 41868
  getItems(final, 3.2) <- "Mtoe"
  
  
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
