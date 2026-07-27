#' calcIDataTechLftPlaType
#'
#' Derive the OPENPROM input parameter iDataTechLftPlaType from the
#' TechCosts2024 PRIMES technology mappings.
#' The dataset contains technical lifetimes for power and heat plant
#' technologies, expressed in years.
#' Technical lifetime values are extracted for the 2020 reference year
#' and mapped to OPENPROM plant type definitions.
#' Missing technologies not available in the source dataset are supplemented
#' using external PRIMES cost assumptions.
#' Missing values are replaced with zero after conversion to magpie format.
#'
#' @return magpie object with OPENPROM input data iDataTechLftPlaType.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataTechLftPlaType", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select filter rename
#' @importFrom tidyr pivot_wider spread gather
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIDataTechLftPlaType <- function() {

  x <- readSource("TechCosts2024", "PowerAndHeat", convert = TRUE)

  # Use PRIMES - OPENPROM mapping to extract correct data from source
  map <- toolGetMapping(name = "prom-primes-pgall-mapping.csv",
                        type = "sectoral",
                        where = "mrprom")
  xq <- as.quitte(x)
  merged <- merge(map, xq, by.x = "PRIMES", by.y = "technology") # INNER JOIN
  variable <- NULL
  period <- NULL
  # Filtering the variable cost per plant type rows and dropping unnecessary columns
  xq <- filter(merged, variable == "Technical lifetime" & period == 2020)
  xq <- xq %>% select(-c("PRIMES", "variable")) %>%
               rename("variable" = "OPEN.PROM")

  # FIXME: Some power plant types are missing from EU Reference Scenario 2020
  # Temporarily adding data from PRIMES_COSTS/techn2009.xlsx
  df_missing <- data.frame(
  variable = c("ATHOIL"),
  model = rep("(Missing)"),
  scenario = rep("(Missing)"),
  region = rep("GLO"),
  unit = rep("Years"),
  period = c(2020),
  value = c(30))
  xq <- rbind(xq, df_missing)

  # Converting to magpie object
  x <- as.quitte(xq) %>% as.magpie()
  # Set NA to 0
  x[is.na(x)] <- 0
  list(x = x,
       weight = NULL,
       unit = "Years",
       description = "EU Reference Scenario 2020; Technical Lifetime per Plant Type")
}
