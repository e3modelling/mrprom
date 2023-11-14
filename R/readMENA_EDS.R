#' readMENA_EDS
#'
#' Read MENA_EDS gdx files, convert it to a MENA_EDS mif file so to compare output mif file
#' with OPEN-PROM output.
#'
#' @return The read-in data into a mif object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("MENA_EDS")
#' }
#'
#' @importFrom gdx readGDX
#' @importFrom dplyr select
#' @importFrom tidyr unite
#' @importFrom quitte as.quitte write.mif interpolate_missing_periods

readMENA_EDS <- function() {
  
  mapping <- c("NEWREG", "ACTVTRP", "PPA", "SCR", "ELCALL","CONSEF", "DEMTR",
               "LFT", "ACTVTRG", "GAPTR", "SFC", "SHRTR", "CONTR", "DEMTRALL",
               "ELCNS", "ELCNSIND", "DEMSE", "ELCHEATPUMP", "PP", "CONSS",
               "GAP", "SHR" , "CONSEF_BAR", "DUMMYOBJ")
  
  mapping_OP <- c("VNewReg", "VTrnspActiv" ,"VFuelPriceAvg", "VScrRate",
                  "VElecConsAll", "VConsFuel", "VDemTr", "VLifeTimeTech",
                  "VGoodsTranspActiv", "VGapTranspFillNewTech",
                  "VSpecificFuelCons", "VTechSortVarCostNewEquip",
                  "VConsEachTechTransp" ,"VFinEneDemTranspSub", "VElecNonSub",
                  "VElecConsInd","VDemSub" ,"VElecConsHeatPla", "VFuelPriceSub",
                  "VConsRemSubEquip", "VGapFinalDem", "VTechShareNewEquip",
                  "VFuelConsInclHP", "vDummyObj")    

  variable <- NULL
  all <- readGDX(gdx = "fulldata.gdx", name = mapping, field = "l")
  names(all) <- mapping_OP
  x <- NULL
  for (j in 1:length(all)) {
    l <- all[j]
    name <- names(l)
    l <- as.quitte(l[[1]])
    l["model"] <- "MENA_EDS"
    d <- names(l)
    l["variable"] <- name
    for (i in 8:length(l)) {
      if (l[1, i] != "NA"){
        l = unite(l, variable, c(variable, d[i]), sep = " ", remove = FALSE)
      }
    }
    l <- select((l), -c(d[8:length(l)]))
    x <- rbind(x, l)
  }
  
  x[which(is.na(x["period"])), 6] <- "2010"
  x["period"] <- as.numeric(unlist(x["period"]))
  x <- interpolate_missing_periods(x, period = 1990:2050, expand.values = TRUE)
  
  x <- as.quitte(x)
  write.mif(x, "MENA_EDS.mif", append = FALSE)
  
  return(suppressWarnings(as.magpie(x)))
}
