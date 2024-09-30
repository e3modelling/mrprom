#' readIAMCOMPACT
#'
#' Read in a XLSX file from IAMCOMPACT and convert it to a magpie object.
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
#' @importFrom readxl read_excel
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
  
 x<- NULL
 y<- NULL
  for (i in (get(subtype))) {
    y <- read_excel(i)
    if ("idx" %in% names(y)) {
      y <- select(y, -c("idx"))
    }
    if ("idx" %in% names(y)) {
      y <- select(y, -c("idx"))
    }
    if ("Category" %in% names(y)) {
      y <- select(y, -c("Category"))
    }
    y <- y %>% pivot_longer(!c("Model", "Scenario", "Region", "Variable", "Unit"), names_to = "period", values_to = "value")

    
    x <- rbind(y ,x)
  }
 
 x[["value"]] <- as.numeric(x[["value"]])
 
 x <- as.quitte(x) %>% as.magpie()
  
  return(x)
 
}
