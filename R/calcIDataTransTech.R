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
  a <- readSource("TechCosts", subtype = "Medium_cars")
  q <- as.quitte(a)
  q$efficiency_value <- sub("_", ".", q$efficiency_value)
  q["efficiency_value"] <- as.numeric(unlist(q["efficiency_value"]))
  
  # load current OPENPROM set configuration
  TRANSFINAL <- readSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "TRANSFINAL")
  TRANSFINAL <- unlist(strsplit(TRANSFINAL[, 1], ","))
  
  TTECH <- readSets(system.file(file.path("extdata", "sets.gms"), package = "mrprom"), "TTECH")
  TTECH <- unlist(strsplit(TTECH[, 1], ","))
  years <- c(2015, 2020, 2030, 2040, 2050)
  x <- as.data.frame(expand.grid(TTECH, TRANSFINAL, years))
  x["IC"] <- NA
  
  index1 <- which(x$Var2 == "PC")
  index2 <- which(x$Var1 == "GSL")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.3)
  index4 <- which(q$efficiency_value < 0.38)
  index5 <- which(q$variable == "Capital cost of ICE medium size gasoline car in Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "PC")
  index2 <- which(x$Var1 == "CHEVGSL")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.4)
  index4 <- which(q$efficiency_value < 0.45)
  index5 <- which(q$variable == "Capital cost of ICE medium size hybrid gasoline car in Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "PC")
  index2 <- which(x$Var1 == "PHEVGSL")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.74)
  index4 <- which(q$efficiency_value < 0.75)
  index5 <- which(q$variable == "Capital cost of ICE medium size plug-in hybrid gasoline car in Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "PC")
  index2 <- which(x$Var1 == "GDO")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.32)
  index4 <- which(q$efficiency_value < 0.38)
  index5 <- which(q$variable == "Capital cost of ICE medium size diesel car in Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "PC")
  index2 <- which(x$Var1 == "CHEVGDO")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.34)
  index4 <- which(q$efficiency_value < 0.40)
  index5 <- which(q$variable == "Capital cost of ICE medium size hybrid diesel car in Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "PC")
  index2 <- which(x$Var1 == "PHEVGDO")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.72)
  index4 <- which(q$efficiency_value < 0.73)
  index5 <- which(q$variable == "Capital cost of ICE medium size plug-in hybrid diesel car in Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "PC")
  index2 <- which(x$Var1 == "NGS")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.32)
  index4 <- which(q$efficiency_value < 0.35)
  index5 <- which(q$variable == "Capital cost of ICE medium size CNG car in Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "PC")
  index2 <- which(x$Var1 == "LPG")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.32)
  index4 <- which(q$efficiency_value < 0.35)
  index5 <- which(q$variable == "Capital cost of ICE medium size LPG car in Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "PC")
  index2 <- which(x$Var1 == "ETH")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.32)
  index4 <- which(q$efficiency_value < 0.35)
  index5 <- which(q$variable == "Capital cost of ICE medium size E85 flex-fuel car in Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "PC")
  index2 <- which(x$Var1 == "ELC")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 270)
  index4 <- which(q$efficiency_value < 320)
  index5 <- which(q$variable == "Capital cost of Medium size battery electric car in Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "PC")
  index2 <- which(x$Var1 == "H2F")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.77)
  index4 <- which(q$efficiency_value < 0.8)
  index5 <- which(q$variable == "Capital cost of Medium size hydrogen fuel cell in Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  a <- readSource("TechCosts", subtype = "Rail")
  q <- as.quitte(a)
  q$efficiency_value <- sub("_", ".", q$efficiency_value)
  q["efficiency_value"] <- as.numeric(unlist(q["efficiency_value"]))
  
  
  index1 <- which(x$Var2 == "PT")
  index2 <- which(x$Var1 == "GDO")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.2)
  index4 <- which(q$efficiency_value < 0.25)
  index5 <- which(q$variable == "Capital cost of diesel passenger rail in Million Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "PT")
  index2 <- which(x$Var1 == "ELC")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.33)
  index4 <- which(q$efficiency_value < 0.39)
  index5 <- which(q$variable == "Capital cost of electric passenger rail in Million Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "GT")
  index2 <- which(x$Var1 == "GDO")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.3)
  index4 <- which(q$efficiency_value < 0.35)
  index5 <- which(q$variable == "Capital cost of diesel freight rail in Million Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "GT")
  index2 <- which(x$Var1 == "ELC")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.32)
  index4 <- which(q$efficiency_value < 0.37)
  index5 <- which(q$variable == "Capital cost of electric freight rail in Million Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "GT")
  index2 <- which(x$Var1 == "H2F")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 9.5)
  index4 <- which(q$efficiency_value < 10)
  index5 <- which(q$variable == "Capital cost of hydrogen fuel cell freight rail in Million Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "PT")
  index2 <- which(x$Var1 == "H2F")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 6)
  index4 <- which(q$efficiency_value < 6.2)
  index5 <- which(q$variable == "Capital cost of hydrogen fuel cell passenger rail in Million Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  a <- readSource("TechCosts", subtype = "Aviation")
  q <- as.quitte(a)
  q$efficiency_value <- sub("_", ".", q$efficiency_value)
  q["efficiency_value"] <- as.numeric(unlist(q["efficiency_value"]))
  
  index1 <- which(x$Var2 == "PA")
  index2 <- which(x$Var1 == "KRS")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.2)
  index4 <- which(q$efficiency_value < 0.25)
  index5 <- which(q$variable == "Capital cost of Conventional aircraft in Million Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "PA")
  index2 <- which(x$Var1 == "H2F")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 2200)
  index4 <- which(q$efficiency_value < 2350)
  index5 <- which(q$variable == "Capital cost of hydrogen fuel cell Aircraft in Million Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  if (length(index9) == 0) {
    index9 <- NA
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "PA")
  index2 <- which(x$Var1 == "ELC")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 2200)
  index4 <- which(q$efficiency_value < 2350)
  index5 <- which(q$variable == "Capital cost of hydrogen fuel cell Aircraft in Million Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  if (length(index9) == 0) {
    index9 <- NA
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "PA")
  index2 <- which(x$Var1 == "ELC")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 400)
  index4 <- which(q$efficiency_value < 600)
  index5 <- which(q$variable == "Capital cost of Battery electric aircraft in Million Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  if (length(index9) == 0) {
    index9 <- NA
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  a <- readSource("TechCosts", subtype = "Inland_navigation")
  q <- as.quitte(a)
  q$efficiency_value <- sub("_", ".", q$efficiency_value)
  q["efficiency_value"] <- as.numeric(unlist(q["efficiency_value"]))
  
  index1 <- which(x$Var2 == "GN")
  index2 <- which(x$Var1 == "GDO")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.17)
  index4 <- which(q$efficiency_value < 0.19)
  index5 <- which(q$variable == "Capital cost of diesel/fuel oil freight inland navigation/national maritime vessel in Million Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  if (length(index9) == 0) {
    index9 <- NA
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "GN")
  index2 <- which(x$Var1 == "ELC")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 180)
  index4 <- which(q$efficiency_value < 210)
  index5 <- which(q$variable == "Capital cost of Battery electric freight vessel in Million Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  if (length(index9) == 0) {
    index9 <- NA
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "GN")
  index2 <- which(x$Var1 == "H2F")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 5.15)
  index4 <- which(q$efficiency_value < 5.2)
  index5 <- which(q$variable == "Capital cost of Hydrogen fuel cell freight vessel in Million Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  if (length(index9) == 0) {
    index9 <- NA
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  a <- readSource("TechCosts", subtype = "HGVs>16t")
  q <- as.quitte(a)
  q$efficiency_value <- sub("_", ".", q$efficiency_value)
  q["efficiency_value"] <- as.numeric(unlist(q["efficiency_value"]))
  
  index1 <- which(x$Var2 == "GU")
  index2 <- which(x$Var1 == "GDO")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.17)
  index4 <- which(q$efficiency_value < 0.19)
  index5 <- which(q$variable == "Capital cost of 16-32t diesel truck in Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
  index9 <- Reduce(intersect, list(index5, index6, index7))
  index12 <- which(x$Var3 == 2015)
  index13 <- Reduce(intersect, list(index12, index1, index2))
  if (length(Reduce(intersect, list(index6, index5, index7))) == 0) {
    index13 <- NULL
  }
  if (length(index9) == 0) {
    index9 <- NA
  }
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "GU")
  index2 <- which(x$Var1 == "CHEVGDO")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.19)
  index4 <- which(q$efficiency_value < 0.21)
  index5 <- which(q$variable == "Capital cost of 16-32t diesel hybrid truck in Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
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
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "GU")
  index2 <- which(x$Var1 == "LPG")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 0.17)
  index4 <- which(q$efficiency_value < 0.19)
  index5 <- which(q$variable == "Capital cost of 16-32t LPG truck in Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
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
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "GU")
  index2 <- which(x$Var1 == "ELC")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 230)
  index4 <- which(q$efficiency_value < 240)
  index5 <- which(q$variable == "Capital cost of 16-32t battery electric truck in Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
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
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
  index1 <- which(x$Var2 == "GU")
  index2 <- which(x$Var1 == "H2F")
  index10 <- which(x$Var3 != 2015)
  index11 <- Reduce(intersect, list(index1, index2, index10))
  index3 <- which(q$efficiency_value > 5)
  index4 <- which(q$efficiency_value < 5.2)
  index5 <- which(q$variable == "Capital cost of 16-32t Hydrogen fuel cell truck in Euro'15")
  index6 <- which(q$period == 2015)
  index7 <- which(!(is.na(q$value)))
  index8 <- Reduce(intersect, list(index3, index4, index5, index7))
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
  x[c(index13, index11), 4] <- q[c(index9, index8), 7]
  
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