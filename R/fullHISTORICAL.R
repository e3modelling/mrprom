#' fullHISTORICAL
#'
#' Read files, convert it to a mif file so to compare mrprom output mif file
#' with OPEN-PROM output.
#'
#' @return The mif file
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- retrieveData("Historical", regionmapping = "regionmappingOPDEV2.csv")
#' }
#'
#' @importFrom quitte as.quitte write.mif
#' @importFrom dplyr select left_join filter
#' @importFrom stringr word
#'
#' @export

fullHISTORICAL <- function() {
  
  y <- NULL
  variable2 <- NULL
  variable <- NULL
  variable3 <- NULL
  SBS <- NULL
  EF <- NULL
  k <- NULL
  
  z <- readSource("ENERDATA", "consumption", convert = TRUE)
  
  for (i in c("NENSE", "DOMSE", "INDSE","TRANSE")) {

    # filter years
    fStartHorizon <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartHorizon"]
    fStartY <- readEvalGlobal(system.file(file.path("extdata", "main.gms"), package = "mrprom"))["fStartY"]
    x <- z[, c(fStartHorizon:fStartY), ]
    
    # load current OPENPROM set configuration
    sets <- readSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), i)
    sets <- unlist(strsplit(sets[,1],","))
    
    # use enerdata-openprom mapping to extract correct data from source
    map <- toolGetMapping(name = "prom-enerdata-fucon-mapping.csv",
                          type = "sectoral",
                          where = "mappingfolder")
    
    ## filter mapping to keep only XXX sectors
    map <- filter(map, map[, "SBS"] %in% sets)
    ## ..and only items that have an enerdata-prom mapping
    enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
    map <- map[map[, "ENERDATA"] %in% enernames, ]
    ## filter data to keep only XXX data
    enernames <- unique(map[!is.na(map[, "ENERDATA"]), "ENERDATA"])
    x <- x[, , enernames]
  
    x[is.na(x)] <- 0
    xq <- as.quitte(x)
    map[, 1] <- word(map[, 1], sep = fixed("."), 1)
    xq <- left_join(xq, map, by=c( "variable" = "ENERDATA"))
    
    xq["variable2"] <- "fuel consumption"
    xq["variable3"] <- i
    xq$variable <- paste(xq$variable2, xq$SBS, xq$EF, xq$variable3)
    xq <- select((xq), -c(SBS, EF, variable2, variable3))
    xq["model"] <- "ENERDATA"
    y <- rbind(y, xq)
  }
  y <- as.quitte(y)
  write.mif(y, 'Data_validation_mrprom.mif', append = FALSE)
  
}