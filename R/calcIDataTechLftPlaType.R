#' calcIDataTechLftPlaType
#'
#' Use data from EU Reference Scenario to derive OPENPROM input parameter iDataTechLftPlaType
#' This dataset includes technical lifetime per plant type, in years.
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
  variable = c("ATHOIL", "ATHBMSCCS"),
  model = rep("(Missing)", 2),
  scenario = rep("(Missing)", 2),
  region = rep("GLO", 2),
  unit = rep("Years", 2),
  period = c(2020, 2020),
  value = c(30, 30))
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
