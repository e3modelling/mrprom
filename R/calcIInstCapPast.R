#' calcIInstCapPast
#'
#' Use data from ENERDATA to derive OPENPROM input parameter iInstCapPast
#' This dataset contains the values of installed capacity for past years in GW.
#'
#' @return  OPENPROM input data iInstCapPast
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IInstCapPast", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate left_join case_when if_else arrange
#' @importFrom tidyr pivot_wider spread gather replace_na
#' @importFrom quitte as.quitte
#' @importFrom tibble add_row

calcIInstCapPast <- function() {
  x <- readSource("ENERDATA", "capacity", convert = TRUE)

  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  years <- getYears(x, as.integer = TRUE)
  x <- x[, c(max(fStartHorizon, min(years)):max(years)), ]

  # use enerdata-openprom mapping to extract correct data from source
  map <- toolGetMapping(
    name = "prom-enerdata-pgall-mapping.csv",
    type = "sectoral",
    where = "mrprom"
  )
  ## ..and only items that have an enerdata-prom mapping
  enernames <- unique(map[!is.na(map[, "ENERDATA..MW."]), "ENERDATA..MW."])
  map <- map[map[, "ENERDATA..MW."] %in% enernames, ]

  enernames <- unique(map[!is.na(map[, "ENERDATA..MW."]), "ENERDATA..MW."])
  enernames <- enernames[!enernames %in% c("")]

  x <- x[,,enernames]
  
  ## rename variables from ENERDATA to openprom names
  getNames(x) <- map[!(map[, 2] == ""), 1]

  # Multiplying the capacity values by the availability rate
  avail <- calcOutput(type = "IAvailRate", aggregate = FALSE)
  avail_rates <- as.quitte(avail["GLO", "y2020", ])[c("variable", "value")]
  years <- getYears(x, as.integer = TRUE)

  EffCapacities <- as.quitte(x) %>%
    replace_na(list("value" = 0)) %>%
    interpolate_missing_periods(period = years, expand.values = TRUE) %>%
    left_join(avail_rates, by = "variable") %>%
    # Applying avail rates & converting MW values to GW
    mutate(value = value.x * value.y / 1000) %>%
    select(c("region", "variable", "period", "value")) %>%
    as.quitte() %>%
    as.magpie()

  list(
    x = EffCapacities,
    weight = NULL,
    unit = "GW",
    description = "Enerdata; Installed capacity"
  )
}
