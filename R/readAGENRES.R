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
#' @importFrom stringr str_detect
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
    type = groups,
    variable = activities
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
  
  x["unit"] <- "energy use per activity"
  x[["value"]] <- as.numeric(x[["value"]])
  x <- as.quitte(x)
  
  levels(x[["region"]]) <- toolCountry2isocode(levels(x[["region"]]), mapping =
                                                 c("World" = "GLO"))
  
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "AGENRES",
                       type = "energy use per activity",
                       filename = "WP1 Dataset.xlsx",
                       `Indicative size (MB)` = 8,
                       dimensions = "3D",
                       unit = "energy use per activity",
                       Confidential = "E3M"))
}
