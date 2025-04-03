#' readIAMCOMPACT
#'
#' Read in a XLSX file from IAMCOMPACT and convert it to a magpie object.
#' 
#' @param subtype The type is referring to the study
#' 
#'  Available types are:
#' \itemize{
#' \item `study0`:
#' \item `study1`:
#' \item `study2`:
#' \item `study3`:
#' \item `study4`:
#' \item `study6`:
#' \item `study7`:
#' \item `all`:
#' }
#'
#' @return magpie object with the requested output data
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IAMCOMPACT", subtype = "study1")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>%  select
#' @importFrom readxl read_excel excel_sheets
#' @importFrom tidyr pivot_longer
#'

readIAMCOMPACT <- function(subtype = "study1") {

 study1 <- c("IAM COMPACT_Study_1_GCAM.xlsx", "IAM COMPACT_Study_1_PROMETHEUS.xlsx",
             "IAM COMPACT_Study_1_TIAM.xlsx")
 
 study2 <- c("1_5 tech IAMC format.xlsx", "Baseline 2050 IAMC format.xlsx",
             "Smart Energy Europe IAMC format.xlsx")
  
 study3 <- c("IAM COMPACT_Study_3_GCAM.xlsx", "IAM COMPACT_Study_3_PROMETHEUS.xlsx.xlsx",
             "IAM COMPACT_Study_3_TIAM.xlsx")
 
 study4 <- c("IAM COMPACT_Study_4_EDM-I_EU-Steel_Results.xlsx", "IAM COMPACT_Study_4_GCAM.xlsx",
             "IAM COMPACT_Study_4_TIAM.xlsx")
 
 study6 <- c("IAM COMPACT_Study_6_GCAM.xlsx", "IAM COMPACT_Study_6_OSeMOSYS_GR.xlsx",
             "IAM COMPACT_Study_6_TIAM.xlsx")
 
 study7 <- c("IAM COMPACT_Study_7_GCAM.xlsx")
 
 study0 <- c("COMPACT_PROMETHEUS_Energy_crisis_v2.xlsx", "compact_TIAM_v3_060323.xlsx",
             "GCAM_energycrisis_v0_5.xlsx", "MUSEcrisis_v02_Feb2023.xlsx")
  
 all <- c("IAM COMPACT_Study_1_GCAM.xlsx", "IAM COMPACT_Study_1_PROMETHEUS.xlsx",
          "IAM COMPACT_Study_1_TIAM.xlsx", "1_5 tech IAMC format.xlsx",
          "Baseline 2050 IAMC format.xlsx", "Smart Energy Europe IAMC format.xlsx",
          "IAM COMPACT_Study_3_GCAM.xlsx", "IAM COMPACT_Study_3_PROMETHEUS.xlsx.xlsx",
          "IAM COMPACT_Study_3_TIAM.xlsx", "IAM COMPACT_Study_4_EDM-I_EU-Steel_Results.xlsx",
          "IAM COMPACT_Study_4_GCAM.xlsx", "IAM COMPACT_Study_4_TIAM.xlsx",
          "IAM COMPACT_Study_6_GCAM.xlsx", "IAM COMPACT_Study_6_OSeMOSYS_GR.xlsx",
          "IAM COMPACT_Study_6_TIAM.xlsx", "IAM COMPACT_Study_7_GCAM.xlsx",
          "compact_TIAM_v3_060323.xlsx", "GCAM_energycrisis_v0_5.xlsx",
          "MUSEcrisis_v02_Feb2023.xlsx", "COMPACT_PROMETHEUS_Energy_crisis_v2.xlsx")
 
 study <- data.frame(
   "subtype" = all,
   "study" = c("Study_1","Study_1","Study_1",
               "Study_2","Study_2","Study_2",
               "Study_3","Study_3","Study_3",
               "Study_4","Study_4","Study_4",
               "Study_6","Study_6","Study_6",
               "Study_7","Study_0","Study_0",
               "Study_0", "Study_0"))
 
 x <- NULL
 y <- NULL
 model <-  NULL
  for (i in (get(subtype))) {
      
      model <- lapply(excel_sheets(i), read_excel, path = i)
      
      for (z in 1 : length(model)) {
        
        y <- as.data.frame(model[z])
        
        if (i == "IAM COMPACT_Study_7_GCAM.xlsx") {
          model <- model[1]
        }
        
        if (length(y) != 0) {
          
          y[["Study"]] <- study[which(study[,1] == i),2]
          
          if ("idx" %in% names(y)) {
            y <- select(y, -c("idx"))
          }
          if ("idx" %in% names(y)) {
            y <- select(y, -c("idx"))
          }
          if ("Category" %in% names(y)) {
            y <- select(y, -c("Category"))
          }
          
          names(y)[names(y) == "Unit"] <- "unit"
          names(y)[names(y) == "model"] <- "Model"
          names(y)[names(y) == "scenario"] <- "Scenario"
          names(y)[names(y) == "region"] <- "Region"
          
          if ("Commodity" %in% names(y)) {
            y <- select(y, -c("Commodity"))
          }
          
          y <- y %>% pivot_longer(names(y)[!(names(y) %in% c("Model", "Scenario", "Region", "Variable", "unit", "Study"))], names_to = "period", values_to = "value")
          
          y[["period"]] <- gsub("X", "", y[["period"]])
          
          y[["value"]] <- as.numeric(y[["value"]])
          
          y <- as.quitte(y)
          
          x <- rbind(y ,x)
        }
      } 
  }
 
 if (subtype %in% c("all", "study4")) {
   x[which(x[["model"]] == "(Missing)"),1] <- "TIAM_Grantham"
   x[which(x[["model"]] == "GCAM7.0"),1] <- "GCAM 7.0"
 }
 
 x[["value"]] <- as.numeric(x[["value"]])
 

 x <- as.quitte(x) %>% as.magpie()
 
  return(x)
 
}
# Things to do when you write the object to csv
# x[which(x[, 5] == "Index (2005 = 1)"), 5] <- "Index"
# x[which(x[, 4] == "Final Energy|Transportation (w/ bunkers)"), 4] <- "Final Energy|Transportation w bunkers"
# x[which(x[, 4] == "Emissions|CO2 (w/o bunkers)"), 4] <- "Emissions|CO2 w o bunkers"
# x[which(x[, 4] == "Emissions|CO2|Energy|Demand|Transportation (w/ bunkers)"), 4] <- "Emissions|CO2|Energy|Demand|Transportation w bunkers"
# x[which(x[, 4] == "Final Energy (w/o bunkers)"), 4] <- "Final Energy w o bunkers"
# needs to take out comma € / t finished steel production, EU avearge
# write.report(x, file ="test.csv",extracols="study")
