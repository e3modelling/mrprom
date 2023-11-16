#' calcIDataTransTech
#'
#' Use data to derive OPENPROM input parameter iDataTransTech
#'
#' @return  OPENPROM input data iDataTransTech
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Giannis Tolios
#'
#' @examples
#' \dontrun{
#' a <- calcOutput(type = "IDataTransTech", aggregate = FALSE)
#' }
#'
#' @importFrom dplyr intersect %>%
#' @importFrom quitte as.quitte interpolate_missing_periods

calcIDataTransTech <- function() {
  
  a1 <- readSource("TechCosts", subtype = "Medium_cars")
  a2 <- readSource("TechCosts", subtype = "Rail")
  a3 <- readSource("TechCosts", subtype = "Aviation")
  a4 <- readSource("TechCosts", subtype = "Inland_navigation")
  a5 <- readSource("TechCosts", subtype = "HGVs>16t")
  
  q <- mbind(a1, a2, a3, a4, a5)
  q <- as.quitte(q)
  q$efficiency_value <- sub("_", ".", q$efficiency_value)
  q["efficiency_value"] <- as.numeric(unlist(q["efficiency_value"]))
  
  map <- toolGetMapping(name = "iDataTransTech-mapping.csv",
                        type = "sectoral",
                        where = "mappingfolder")
  # load current OPENPROM set configuration
  TRANSFINAL <- readSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "TRANSFINAL")
  TRANSFINAL <- unlist(strsplit(TRANSFINAL[, 1], ","))
  
  TTECH <- readSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "TTECH")
  TTECH <- unlist(strsplit(TTECH[, 1], ","))
  years <- c(2015, 2020, 2030, 2040, 2050)
  x <- as.data.frame(expand.grid(TTECH, TRANSFINAL, years))
  
  mymedian <- function(lst) {
    n <- length(lst)
    s <- sort(lst)
    ifelse(n%%2==1,s[(n+1)/2],(s[n/2]))
  }
  q <- mutate(q, mean_of_eff = mymedian(efficiency_value), .by = c("variable"))
  
  for (i in 1:nrow(map)) {
    index1 <- which(x$Var2 == map[i, 2])
    index2 <- which(x$Var1 == map[i, 1])
    index10 <- which(x$Var3 != 2015)
    index11 <- Reduce(intersect, list(index1, index2, index10))
    index3 <- which(q$efficiency_value == q$mean_of_eff)
    index5 <- which(q$variable == map[i, 3])
    index6 <- which(q$period == 2015)
    index7 <- which(!(is.na(q$value)))
    index8 <- Reduce(intersect, list(index3, index5, index7))
    index9 <- Reduce(intersect, list(index5, index6, index7))
    index12 <- which(x$Var3 == 2015)
    index13 <- Reduce(intersect, list(index12, index1, index2))
    if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
      index13 <- NULL
    }
    if (length(index9) == 0) {
      index9 <- NA
    }
    if (length(index13) == 0) {
      index9 <- NULL
    }
    if (length(index5) == 0) {
      index13 <- NULL
      index11 <- NULL
      index9 <- NULL
      index8 <- NULL
    }
    if (length(index8) == 3) {
      index11 <-index11[-1]
    }
    x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  }
  
  names(x) <- c("TTECH", "TRANSFINAL" ,"period", "value")
  
  index1 <- which(x$TTECH == "KRS")
  index2 <- which(x$TRANSFINAL == "PC")
  index3 <- Reduce(intersect, list(index1, index2))
  x <- x[ - index3, ]
  
  index1 <- which(x$TTECH %in% c("GSL", "LPG", "NGS", "KRS", "ETH", "CHEVGDO", "BGDO", "PHEVGSL",
                               "PHEVGDO", "CHEVGSL"))
  index2 <- which(x$TRANSFINAL == "PT")
  index3 <- Reduce(intersect, list(index1, index2))
  x <- x[ - index3, ]
  
  index1 <- which(x$TTECH %in% c("GSL", "LPG", "NGS", "GDO", "ELC", "ETH", "MET", 
                               "BGDO", "PHEVGSL", "PHEVGDO","CHEVGSL", "CHEVGDO"))
  index2 <- which(x$TRANSFINAL == "PA")
  index3 <- Reduce(intersect, list(index1, index2))
  x <- x[ - index3, ]
  
  index1 <- which(x$TTECH %in% c("KRS", "CHEVGSL"))
  index2 <- which(x$TRANSFINAL == "GU")
  index3 <- Reduce(intersect, list(index1, index2))
  x <- x[ - index3, ]
  
  index1 <- which(x$TTECH %in% c("GSL", "LPG", "NGS", "KRS", "ETH", 
                               "BGDO", "PHEVGSL", "PHEVGDO","CHEVGSL", "CHEVGDO"))
  index2 <- which(x$TRANSFINAL == "GT")
  index3 <- Reduce(intersect, list(index1, index2))
  x <- x[ - index3, ]
  
  index1 <- which(x$TTECH %in% c("LPG", "NGS", "ELC", "KRS", "ETH", "MET",
                               "BGDO", "PHEVGSL", "PHEVGDO","CHEVGSL", "CHEVGDO"))
  index2 <- which(x$TRANSFINAL == "GN")
  index3 <- Reduce(intersect, list(index1, index2))
  x <- x[ - index3, ]
  
  x <- as.quitte(x) %>%
    interpolate_missing_periods(period = 2010:2100, expand.values = TRUE)
  x <- as.quitte(x)
  x["variable"] <- "IC"
  x <- as.magpie(x)
  # set NA to 0
  x[is.na(x)] <- 0
  
  return(list(x = x,
              weight = NULL,
              unit = NULL,
              description = "readTechCosts;"))
}