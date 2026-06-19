#' readTINDSE2
#'
#' @param subtype string.
#'
#' @return magpie object
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' TTINDSE2 <- readSource("TINDSE2")
#' }
#'
#' @importFrom quitte as.quitte interpolate_missing_periods
#' @importFrom dplyr %>% filter mutate group_by ungroup full_join select rename
#'  

readTINDSE2 <- function() {
  
  a <- readSource("TSharesINDSE", subtype = "PrimesShares")
  a[is.na(a)] <- 0
  b <- readSource("TSharesINDSE", subtype = "IEAShares")
  b <- add_columns(b, addnm = "BGAS", dim = 3.2, fill = 0)
  x <- mbind(a, b)
  
  ##### disaggregation bio to fuel level
  itemsx <- getItems(x[,,c("BGDO","BGSL","BKRS","BMSWAS","BGAS")], 3)
  BIO <- dimSums(x[,,c("BGDO","BGSL","BKRS","BMSWAS","BGAS")], 3.2, na.rm = TRUE)
  BIO <- add_dimension(BIO, dim = 3.2, nm = c("SUM"), add = "fuel")
  IFuelCons2 <- calcOutput(type = "IFuelCons2", aggregate = TRUE, regionmapping = "regionmappingOPDEV5.csv")
  IFuelCons2BIO <- IFuelCons2[,,getItems(BIO,3.1)]
  IFuelCons2BIO <- IFuelCons2BIO[,,c("BGDO","BGSL","BKRS","BMSWAS","BGAS")]
  IFuelCons2BIO <- add_columns(IFuelCons2BIO, addnm = setdiff(itemsx, getItems(IFuelCons2BIO,3)), dim = 3, fill = 0)
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
  
  # z <- dimSums(a, 3.2, na.rm = TRUE)
  # z <- filter(as.quitte(z), value == 0, period == 2024)
  # zx <- a[unique(z[["region"]]),,unique(z[["variable"]])]
  # 
  # a[getItems(zx,1),,getItems(zx,3)] <- 1/25 # Assuming 25 fuels, we assign an equal share
  
  x[is.na(x)] <- 0
    
  
  list(x = x,
       weight = NULL,
       description = c(category = "TINDS2E",
                       type = "TINDSE2",
                       filename = "TINDSE2",
                       `Indicative size (MB)` = 0,
                       dimensions = "3D",
                       unit = "shares, ",
                       Confidential = "E3M"))
}
