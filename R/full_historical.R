#' full_historircal
#'
#' Creates .mif objects from calcoutput
#'
#' @return The .mif objects
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- full_historircal()
#' }
#'
#' @importFrom quitte as.quitte write.mif
#' @importFrom dplyr select left_join filter
#'
#' @export

full_historircal <- function() {
  y <- NULL
  variable2 <- NULL
  variable <- NULL
  variable3 <- NULL
  new <- NULL
  for (i in c("NENSE", "DOMSE", "INDSE")) {
    x <- calcOutput(type = "IFuelCons", i, aggregate = TRUE)
    sets <- readSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), i)
    sets <- unlist(strsplit(sets[,1],","))
    
    # use enerdata-openprom mapping to extract correct data from source
    map <- toolGetMapping(name = "prom-enerdata-fucon-mapping.csv",
                          type = "sectoral",
                          where = "mappingfolder")
    x[is.na(x)] <- 0
    xq <- as.quitte(x)
    xq <- left_join(xq, map, by=c( "variable" = "SBS", "new" = "EF"))
    xq["variable2"] <- "fuel consumption"
    xq["variable3"] <- i
    xq$variable <- paste(xq$variable2, xq$variable, xq$new, xq$variable3)
    xq <- select((xq), -c(new, variable2, variable3))
    f <- 'G:/.shortcut-targets-by-id/1EMFI-atXDWnU_gNk8RmLscum8s091dix/madratverse/output/fotis'
    y <- rbind(y, xq)
  }
  y <- as.quitte(y)
  write.mif(y, path = f, append = FALSE)
}