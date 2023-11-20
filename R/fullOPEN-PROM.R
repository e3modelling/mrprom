#' fullOPEN-PROM
#'
#' Read in several files with data from GEME3, IRF and WDI_PA and convert it
#' to a csv file.
#' The dataset contains several data types about transport, traffic, air
#' transport passengers per country and per year and Production Level and
#' Unit Cost data.
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @importFrom dplyr %>% select left_join mutate
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_replace
#' @importFrom quitte as.quitte
#' @importFrom utils write.table
#'
#' @examples
#' \dontrun{
#'  a <- retrieveData("OPEN_PROM", regionmapping = "regionmappingOP.csv")
#' }

fullOPEN_PROM <- function() {

  calcOutput(type = "ACTV", file = "iACTV.csvr", aggregate = TRUE)
  calcOutput(type = "POP", file = "iPop.csvr", aggregate = TRUE)
  calcOutput(type = "iGDP", file = "iGDP.csvr", aggregate = TRUE)

  for (i in c("NENSE", "DOMSE", "INDSE", "TRANSE")) {
    x <- calcOutput(type = "IFuelCons", subtype = i, aggregate = TRUE)
    x[is.na(x)] <- 0
    xq <- as.quitte(x) %>%
          select(c("period", "value", "region", "variable", "new")) %>% # nolint
          pivot_wider(names_from = "period") # nolint
    fheader <- paste("dummy,dummy,dummy", paste(colnames(xq)[4 : length(colnames(xq))], collapse = ","), sep = ",")
    writeLines(fheader, con = paste0("iFuelCons", i, ".csv"))
    write.table(xq,
                quote = FALSE,
                row.names = FALSE,
                file = paste0("iFuelCons", i, ".csv"),
                sep = ",",
                col.names = FALSE,
                append = TRUE)
  }

  x <- calcOutput("IFuelPrice", aggregate = FALSE)
  # compute weights for price aggregation
  map <- toolGetMapping(getConfig("regionmapping"), "regional")
  qx <- as.quitte(x)
  names(qx) <- sub("region", "ISO3.Code", names(qx))
  ## add mapping to dataset
  qx <- left_join(qx, map, by = "ISO3.Code")
  ## weight value is 1 / (number of non NA values for each year, country, variable, fuel)
  value <- NULL
  qx <- mutate(qx, value = 1 / length(which(!is.na(value))), .by = c("Region.Code", "period", "new", "variable"))
  names(qx) <- sub("ISO3.Code", "region", names(qx))
  qx <- select(qx, -c("model", "scenario", "Full.Country.Name", "Region.Code"))
  weight <- as.magpie(as.quitte(qx))
  # perform price aggregation
  x <- toolAggregate(x, weight = weight, rel = map, from = "ISO3.Code", to = "Region.Code")
  # write input data file that GAMS can read
  xq <- as.quitte(x)
  xq <- xq[!is.na(xq[["value"]]), ] %>%
        select(c("period", "value", "region", "variable", "new")) %>% # nolint
        pivot_wider(names_from = "period") # nolint
  fheader <- paste("dummy,dummy,dummy", paste(colnames(xq)[4 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iFuelPrice.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iFuelPrice.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput("INewReg", aggregate = TRUE)
  # write input data file that GAMS can read
  xq <- as.quitte(x) %>%
        select(c("period", "value", "region")) %>% # nolint
        pivot_wider(names_from = "period") # nolint
  fheader <- paste("dummy", paste(colnames(xq)[2 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iNewReg.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iNewReg.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput("ITransChar", aggregate = FALSE)
  # compute weights for aggregation
  map <- toolGetMapping(getConfig("regionmapping"), "regional")
  qx <- as.quitte(x)
  names(qx) <- sub("region", "ISO3.Code", names(qx))
  ## add mapping to dataset
  qx <- left_join(qx, map, by = "ISO3.Code")
  ## weight value is 1 / (number of non NA values for each year, country, variable, fuel)
  value <- NULL
  qx <- mutate(qx, value = 1 / length(which(!is.na(value))), .by = c("Region.Code", "period", "variable"))
  names(qx) <- sub("ISO3.Code", "region", names(qx))
  qx <- select(qx, -c("model", "scenario", "Full.Country.Name", "Region.Code"))
  weight <- as.magpie(as.quitte(qx))
  # perform price aggregation
  x <- toolAggregate(x, weight = weight, rel = map, from = "ISO3.Code", to = "Region.Code")
  # write input data file that GAMS can read
  xq <- as.quitte(x)
  xq <- xq[!is.na(xq[["value"]]), ] %>%
        select(c("period", "value", "region", "variable")) %>% # nolint
        pivot_wider(names_from = "period") # nolint
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iTransChar.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iTransChar.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput(type = "IDataPassCars", aggregate = FALSE)
# compute weights for aggregation
  map <- toolGetMapping(getConfig("regionmapping"), "regional")
  qx <- as.quitte(x)
  names(qx) <- sub("region", "ISO3.Code", names(qx))
  ## add mapping to dataset
  qx <- left_join(qx, map, by = "ISO3.Code")
  ## weight value is 1 / (number of non NA values for each year, country, variable, fuel)
  value <- NULL
  qx <- mutate(qx, value = 1 / length(which(!is.na(value))), .by = c("Region.Code", "period", "variable"))
  names(qx) <- sub("ISO3.Code", "region", names(qx))
  qx <- select(qx, -c("model", "scenario", "Full.Country.Name", "Region.Code"))
  weight <- as.magpie(as.quitte(qx))
  # perform price aggregation
  x <- toolAggregate(x, weight = weight, rel = map, from = "ISO3.Code", to = "Region.Code")
  a <- as.quitte(x)
  z <- select(a, "region", "unit", "period", "value")
  z <- pivot_wider(z, names_from = "period", values_from = "value")
  fheader <- paste("dummy,dummy,scr")
  writeLines(fheader, con = paste0("iDataPassCars", ".csv"))
  write.table(z[, c(1, 2, 5)],
              quote = FALSE,
              row.names = FALSE,
              file = paste0("iDataPassCars", ".csv"),
              sep = ",",
              col.names = FALSE,
              append = TRUE)

  x <- calcOutput("IDataElecSteamGen", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("period", "value", "region", "variable")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataElecSteamGen.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iDataElecSteamGen.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)
  
  x <- calcOutput("IDataElecAndSteamGen", aggregate = TRUE)
  xq <- as.quitte(x) %>%
    select(c("period", "value", "region", "variable")) %>%
    pivot_wider(names_from = "period")
  fheader <- paste("dummy,dummy", paste(colnames(xq)[3 : length(colnames(xq))], collapse = ","), sep = ",")
  writeLines(fheader, con = "iDataElecAndSteamGen.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iDataElecAndSteamGen.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)
  
  x <- calcOutput("IDataTransTech", aggregate = FALSE)
  x <- as.quitte(x)
  y <- filter(x, variable == "LFT" & x$value > 0)
  x <- x[which(x$variable != "LFT"), ]
  x <- select(x, c("transfinal", "ttech", "value", "variable", "period")) %>%
    pivot_wider(names_from = "period")
  xq <- x
  xq <- left_join(xq, y, by = c( "transfinal", "ttech"))
  xq <- select(xq, -c("model", "scenario", "region", "variable.y", "period", "unit"))
  fheader <- paste("dummy,dummy,dummy", paste(colnames(xq)[4 : (length(colnames(xq))-1)], collapse = ","),"LFT", sep = ",")
  writeLines(fheader, con = "iDataTransTech.csv")
  write.table(xq,
              quote = FALSE,
              row.names = FALSE,
              file = "iDataTransTech.csv",
              sep = ",",
              col.names = FALSE,
              append = TRUE)
  
  return(list(x = x,
              weight = NULL,
              unit = "various",
              description = "OPENPROM input data"))

}
