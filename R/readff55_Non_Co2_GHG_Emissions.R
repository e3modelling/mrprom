#' readff55_Non_Co2_GHG_Emissions
#'
#' Read Non_Co2_GHG_Emissions of EU
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("ff55_Non_Co2_GHG_Emissions", convert = FALSE)
#' }
#'
#' @importFrom utils read.csv
#' @importFrom dplyr filter select distinct
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#' @importFrom readxl read_excel excel_sheets
#'
readff55_Non_Co2_GHG_Emissions <- function() {
  
  sheet_names <- excel_sheets("ff55_reg_non-co2_ghg_emissions.xlsx")
  
  # Read sheets into a named list
  sheets <- lapply(sheet_names, read_excel, path = "ff55_reg_non-co2_ghg_emissions.xlsx")
  
  # Assign names to the list
  names(sheets) <- sheet_names
  
  x <- NULL
  for (i in 2 : length(sheets)) {
    y <- as.data.frame(sheets[i])
    region <- sheet_names[i]
    y <- y[,-c(2:7)]
    y[["region"]] <- region
    y[["unit"]] <- "Non-CO2 GHG in MT CO2eq"
    names(y)[1] <- "variable"
    names(y) <- gsub("^[A-Za-z]+\\.(\\d{4})$", "\\1", names(y))
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
       description = c(category = "Non_Co2_GHG_Emissions",
                       type = "Emissions",
                       filename = "ff55_reg_non-co2_ghg_emissions.xlsx",
                       `Indicative size (MB)` = 0.164,
                       dimensions = "2D",
                       unit = "Non-CO2 GHG in MT CO2eq",
                       Confidential = "project"))
}
