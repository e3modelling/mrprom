#' calcIDataElecAndSteamGen
#'
#' Use data to derive OPENPROM input parameter iDataElecAndSteamGen
#'
#' @return  OPENPROM input data iDataElecAndSteamGen
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataElecAndSteamGen", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr %>% select mutate row_number
#' @importFrom tidyr crossing
#' @importFrom quitte as.quitte
#' @importFrom tibble tibble

calcIDataElecAndSteamGen <- function() {

  # Get time range from GAMS code
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fStartY <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartY"]
  
  # Get CHP set from GAMS code
  sets <- readSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), 'CHP')
  set_chp <- unlist(strsplit(sets[, 1], ","))
  
  # Creating a dummy tibble with CHP capacities for each country
  regions <- unname( getISOlist() )
  variable <- set_chp
  period <- fStartHorizon:fStartY
  
  tibble_chp <- tibble( model = "(missing)",
                        scenario = "(missing)",
                        unit = "(missing)",
                        region = regions,
                        value = 0 )
  
  # expanding the tibble with more columns
  id <- NULL
  tibble_expand <- crossing(period, variable)
  tibble_tmp <- mutate(tibble_chp, id = row_number())
  tibble_chp <- crossing(tibble_tmp, tibble_expand) %>%
                select(-id)

  # Converting to magpie object
  x <- as.quitte(tibble_chp) %>% as.magpie()
  # set NA to 0
  x[is.na(x)] <- 0
  list(x = x,
       weight = NULL,
       unit = "GW",
       description = "Historical CHP Capacity")
}
