#' convertIEA_WEO_TechCosts
#'
#' The ISO codes of "IEA_WEO_TechCosts" data are compared with the official ISO code country list.
#'
#' @param x MAgPIE object.
#'
#' @return The "IEA_WEO_TechCosts" data with spatial entries for each country.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_WEO_TechCosts", subtype = "Renewables", convert = TRUE)
#' }
#'
#' @importFrom dplyr filter select %>% mutate
#' @importFrom quitte as.quitte

convertIEA_WEO_TechCosts <- function(x)
{
  map <- toolGetMapping(name = "map_IEA_WEO_TechCosts.csv",
                        type = "regional",
                        where = "mrprom")
 
  q <- as.quitte(x)
  
  q[["region"]] <- toolCountry2isocode((q[["region"]]), mapping =
                                         c("European Union" = "European Union",
                                           "United States" = "USA",
                                           "Japan" = "JPN",
                                           "Russia" = "RUS",
                                           "China" = "CHN",
                                           "India" = "IND",
                                           "Middle East" = "Middle East",
                                           "Africa" = "Africa",
                                           "Brazil" = "BRA"))
  
  ## filter data
  regions <- unique(q[!is.na(q[, "region"]), "region"])
  
  map <- filter(map, map[["Region.Code"]] %in% regions[["region"]])
  names(map) <- sub("Region.Code", "region", names(map))
  
  q <- left_join(q, map[,2:3], by = c("region")) %>%
    select(-c("region"))
  
  names(q) <- sub("ISO3.Code", "region", names(q))
  
  x <- as.quitte(q) %>% as.magpie()

  x <- toolCountryFill(x, fill = NA)
  
  return(x[as.character(getISOlist()), , ])
  
}
