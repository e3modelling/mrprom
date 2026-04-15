#' readMAGPIE_runs
#'
#' Read the output of MAGPIE.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("MAGPIE_runs")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom quitte as.quitte
#'
readMAGPIE_runs <- function() {
  
  fStartHorizon <- readEvalGlobal(
    system.file(file.path("extdata", "main.gms"), package = "mrprom")
  )["fStartHorizon"]
  
  files <- list.files(".")
  
  magpie <- NULL
  for (i in files) {
    
    # read magpie for this file `i`
    x <- read.report(i)[[1]][[1]]
    
    # add scenario (dim 3.2) with name = i
    x <- add_dimension(x, dim = 3.2, add = "scenario", nm = i)
    
    # bind it
    magpie <- mbind(magpie, x)
  }
  
  items3 <- getItems(magpie, 3)
  items3 <- items3[!is.na(items3)]
  
  get_items <- grep("^Emissions", items3, value = TRUE)
  
  magpie <- magpie[,,get_items]
  
  magpie <- as.quitte(magpie) %>% select(-data) %>% as.magpie()
  
  Globiom <- readSource("GLOBIOMEU", convert = FALSE)
  
  years_GBR <- getYears(Globiom, as.integer = TRUE)
  
  regionOP <- toolGetMapping(name = "regionmappingOPDEV5.csv",
                             type = "regional",
                             where = "mrprom")
  
  regionMag <- toolGetMapping(name = "h12.csv",
                              type = "regional",
                              where = "mrprom")
  
  EU27 <- setdiff(getRegions(Globiom), "EU27")
  inc <- intersect(regionOP[,3],regionMag[,3])
  
  Globiom <- Globiom[EU27,,]
  Globiom <- as.quitte(Globiom) %>% mutate(value = mean(value, na.rm = TRUE), .by = c("region"))
  Globiom <- distinct(Globiom)
  Globiom <- as.quitte(Globiom)
  Globiom <- as.magpie(Globiom)
  Globiom <- Globiom[,2020,]
  Globiom <- dimSums(Globiom,2)
  Globiom_OP <- Globiom[intersect(getRegions(Globiom),regionOP[,3]),,]
  eur_map_op_prom <- Globiom_OP
  
  Emi_GBR <- readSource("UN_GBR_LULUCF")
  years_GBR2 <- getYears(Emi_GBR, as.integer = TRUE)
  years_GBR2 <- years_GBR2[years_GBR2 %in% years_GBR]
  Emi_GBR <- Emi_GBR[,years_GBR2,]
  Emi_GBR <- mean(Emi_GBR)
  eur_map_op_prom <- add_columns(eur_map_op_prom, addnm = c("GBR"), dim = 1, fill = Emi_GBR)
  
  rmap <- data.frame(EUR_24 = rep("EUR", 28),
                     EUR_24_OP = getRegions(eur_map_op_prom))
  
  x_EUR <- toolAggregate(magpie["EUR",,], rel = rmap, weight = eur_map_op_prom)
  x_common <- magpie[inc,,]
  
  x <- mbind(x_EUR, x_common)
  
  x <- x[,getYears(x, as.integer = T) >= fStartHorizon,]
  
  list(x = x,
       weight = NULL,
       description = c(category = "GHG emissions",
                       type = "MAGPIE_runs GHG emissions",
                       filename = "MAGPIE_1P5C.mif",
                       `Indicative size (MB)` = 30,
                       dimensions = "4D",
                       unit = "Mt",
                       Confidential = "E3M"))
}
