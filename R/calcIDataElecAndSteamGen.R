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
#' @importFrom dplyr %>% select mutate
#' @importFrom tidyr crossing
#' @importFrom quitte as.quitte

calcIDataElecAndSteamGen <- function() {

  # filter years
  fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
  fStartY <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartY"]
  
  # Get country codes from mapping 
  map <- toolGetMapping(name = "regionmappingOPDEV2.csv",
                        type = "regional",
                        where = "mappingfolder")
  
  
  # Creating a dummy tibble with CHP capacities for each country
  regions <- map$ISO3.Code
  variable <- c("STE1AL", "STE1AH", "STE1AD", "STE1AR", "STE1AG", "STE1AB")
  period <- fStartHorizon:fStartY
  
  tibble_chp <- tibble( model = "(missing)",
                        scenario = "(missing)",
                        unit = "(missing)",
                        region = regions,
                        value = 0 )
  
  # expanding the tibble with more columns
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
