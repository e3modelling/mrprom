#' readOPEN_GEM
#'
#' Read Production Level and Unit Cost data as delivered in GDX files from GEME3.
#' 
#' @param subset string.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("OPEN_GEM")
#' }
#'
#' @importFrom gdxrrw rgdx.set
#' @importFrom gdx readGDX
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @import mrdrivers

readOPEN_GEM <- function(subset = "INDPROD") {
  .cleanDataAllSets <- function(x) {
    
    x <- as.quitte(x)
    names(x) <- sub("pr", "sector", names(x))
    pr <- rgdx.set("OPEN_GEM_v20261016.gdx", "pr", te = TRUE)
    vctr <- pr
    vctr <- as.data.frame(vctr)
    names(vctr) <- sub("pr", "sector", names(vctr))
    ga <- left_join(x, vctr, by = c("sector")) %>%
      select(-c("sector"))
    names(ga) <- sub(".te", "sector", names(ga))
    return(ga)
  }
  
  x <- readGDX(gdx = "OPEN_GEM_v20261016.gdx", name = c("A_XD", "P_PD", "A_HC", "P_HC", "P_PWE", "A_YVTWR"),
               field = "l", restore_zeros = FALSE)
  
  A_EXPO <- readGDX(gdx = "OPEN_GEM_v20261016.gdx", name = c("A_EXPO"),field = "l", restore_zeros = FALSE)
  A_EXPO <- dimSums(A_EXPO,1.1)
  
  
  ga <- lapply(x, .cleanDataAllSets)
  A_EXPO <- .cleanDataAllSets(A_EXPO)
  
  levels(ga[["A_XD"]][["variable"]]) <- "Production Level"
  levels(ga[["P_PD"]][["variable"]]) <- "Unit Cost"
  levels(ga[["A_HC"]][["variable"]]) <- "Household Consumption"
  levels(ga[["P_HC"]][["variable"]]) <- "End-Use Prices"
  levels(A_EXPO[["variable"]]) <- "Total Exports"
  levels(ga[["P_PWE"]][["variable"]]) <- "Unit Cost Exports"
  levels(ga[["A_YVTWR"]][["variable"]]) <- "Activity Exports"
  
  ga <- rbind(ga[["A_XD"]], ga[["P_PD"]], ga[["A_HC"]], ga[["P_HC"]], A_EXPO, ga[["P_PWE"]], ga[["A_YVTWR"]])
  
  x <- as.magpie(ga)["EU28", , , invert = TRUE]
  
  if (subset == "GDP_POP") {
    
    x <- readGDX(gdx = "OPEN_GEM_v20261016.gdx", name = c("REPORT_MACRO"),
                 field = "l", restore_zeros = FALSE)
    x <- x[, , "EU27", invert = TRUE]
    
    # disaggregate data to all ISO3 countries based on their GDP or GDP per capita
    GDP <- calcOutput("iGDP", aggregate = FALSE) # will use GDP as disaggregation weights
    GDP <- GDP[, getYears(x), , drop = TRUE]
    
    Population <- calcOutput("POP", aggregate = FALSE)
    Population <- Population[, getYears(x), , drop = TRUE]
    
    mapping <- toolGetMapping("country_mapping_OPEN_GEM_249_to_28.csv", type = "regional", where = "mrprom") 
    names(mapping) <- c("GEM.E3.region", "ISO3.Code", "OPEN.PROM.Region")
    # DISSAGREGATION TO COUNTRY LEVEL (WORKS)
    rel <- select(mapping, c("ISO3.Code", "GEM.E3.region")) # iso3-geme3 country mapping
    
    xGDPPOP <- as.quitte(x[, , c("POP", "GDP")]) %>%
      select(period, value, X., X.1) %>%
      rename(
        variable = X.,
        region = X.1
      ) %>% as.quitte() %>% as.magpie()
    
    tmp <- toolAggregate(xGDPPOP[,,"GDP"], rel = rel, weight = GDP , from = "GEM.E3.region", to = "ISO3.Code", dim = 1)
    tmp2 <- toolAggregate(xGDPPOP[,,"POP"], rel = rel, weight = Population , from = "GEM.E3.region", to = "ISO3.Code", dim = 1)
    
    x <- mbind(tmp, tmp2)
    
  }
  
  list(x = x,
       weight = NULL,
       description = c(category = "Costs",
                       type = "Production Level, Unit Cost data, Household Consumption and Exports",
                       filename = "OPEN_GEM_v20261016.gdx",
                       `Indicative size (MB)` = 369,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "E3M"))
}
