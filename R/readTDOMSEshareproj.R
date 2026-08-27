#' readTDOMSEshareproj
#'
#' @param subtype string.
#'
#' @return magpie object
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' TDOMSE <- readSource("TDOMSEshareproj", subtype = "Shares")
#' }
#'
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr %>% filter mutate group_by ungroup full_join select rename
#'  

readTDOMSEshareproj <- function(subtype) {
  
  if (subtype == "Shares") {
    a <- readSource("TSharesDOMSE", subtype = "Shares")
    a <- add_columns(a, addnm = "AG", dim = "variable", fill = NA)
    b <- readSource("TSharesDOMSE", subtype = "PrimesSharesExtension")
    b <- b[, getYears(a),]
    
    IFuelCons2 <- calcOutput(type = "IFuelCons2", aggregate = TRUE, regionmapping = "regionmappingOPDEV5.csv")
    items <- getItems(IFuelCons2, 3.2)
    
    b <- add_columns(b, addnm = setdiff(items, getItems(b,3.2)), dim = "fuel", fill = NA)
    a <- add_columns(a, addnm = setdiff(items, getItems(a,3.2)), dim = "fuel", fill = NA)
    
    x <- mbind(a, b)
    
    ##### disaggregation bio to fuel level
    BIO <- dimSums(x[,,c("BGDO","BGSL","BKRS","BMSWAS","BGAS")], 3.2, na.rm = TRUE)
    BIO <- add_dimension(BIO, dim = 3.2, nm = c("SUM"), add = "fuel")
    IFuelCons2BIO <- IFuelCons2[,,c("BGDO","BGSL","BKRS","BMSWAS","BGAS")][,,getItems(BIO,3.1)]
    IFuelCons2BIO <- add_columns(IFuelCons2BIO, addnm = "BKRS", dim = 3.2, fill = NA)
    IFuelCons2BIO[is.na(IFuelCons2BIO)] <- 0
    
    mapBIO <- data.frame(
      BIO = c("BGDO","BGSL","BKRS","BMSWAS","BGAS"),
      AGG = rep("SUM", 5),
      stringsAsFactors = FALSE
    )
    
    IFuelCons2BIO <- IFuelCons2BIO[,2023,]
    IFuelCons2BIO <- collapseDim(IFuelCons2BIO, 2)
    
    BIO <- toolAggregate(BIO, weight = IFuelCons2BIO, dim = 3.2, rel = mapBIO, from = "AGG", to = "BIO")
    
    x[,,getItems(BIO,3)] <- BIO
    
  }
  
  if (subtype == "Projections") {
    IFuelCons2 <- calcOutput(type = "IFuelCons2", aggregate = TRUE, regionmapping = "regionmappingOPDEV5.csv")
    IFuelCons2 <- dimSums(IFuelCons2, 3.2)[,,c("AG", "SE", "HOU")]
    c <- readSource("TSharesDOMSE", subtype = "Projections")
    d <- readSource("TSharesDOMSE", subtype = "growthPrimesBalancesStepWiseExtension")
    c <- add_columns(c, addnm = "AG", dim = "variable", fill = NA)
    x <- mbind(c, d)
    x <- as.quitte(x)
    IFuelCons2 <- as.quitte(IFuelCons2) %>%
      select(-c("variable")) %>% rename(variable = "dsbs") %>%
      interpolate_missing_periods(period = seq(2010, 2100, 1), expand.values = TRUE)
    
    y <- as.quitte(x) %>%
      group_by(region, variable) %>%
      mutate(value = cumprod(1 +value)) %>% ungroup()
    
    combinedf <- full_join(y, IFuelCons2, by = c("region", "period", "variable", "model", "scenario", "unit")) %>%
      mutate(value = ifelse(period <= 2023, value.y, value.x * value.y))  %>%
      select(region, period, variable, value) %>% filter(period >= 2024)
    
    x <- as.quitte(combinedf)
    x <- as.magpie(x)
    
  }
  
  list(x = x,
       weight = NULL,
       description = c(category = "TDOMSE",
                       type = "TDOMSE",
                       filename = "TDOMSE",
                       `Indicative size (MB)` = 0,
                       dimensions = "3D",
                       unit = "shares, projections",
                       Confidential = "E3M"))
}
