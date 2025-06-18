#' readIEA_Energy_End_uses_and_Efficiency_Indicators
#'
#' The Energy end-uses and efficiency indicators database contains annual data
#' from 2000 to 2021, covering end-use energy consumption by energy product, 
#' end use carbon emissions, associated indicators across the four main sectors of final
#' consumption (residential, services, industry and transport), and decomposition
#' analysis data, for IEA member countries and beyond.The data includes
#' "ENDUSE", "COUNTRY", "FUEL","METRIC", "TIME", "ACTIVITY", "ENERGY",
#' "EMISSIONS", "SECTORS"
#'
#' @param subtype fuel : Type of data that should be read.
#' Available types are:
#' \itemize{
#' \item `GENERIC`
#' \item `OIL`
#' \item `NATGAS`
#' \item `COAL`
#' \item `COMRENEW`
#' \item `HEAT`
#' \item `ELECTR`
#' \item `OTHER`
#' \item `TOTAL`
#' }
#' 
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("IEA_Energy_End_uses_and_Efficiency_Indicators", subtype = "GENERIC", convert = TRUE)
#' }
#'
#' @importFrom utils read.csv2
#' @importFrom dplyr filter
#' @importFrom quitte as.quitte
#' @importFrom tidyr separate_wider_delim
#'
readIEA_Energy_End_uses_and_Efficiency_Indicators <- function(subtype = "GENERIC") {
  
  if (!file.exists("IEA_Energy_End_uses_and_Efficiency_Indicators.rds")) {
    x1 <- read.csv2("EEI_TRANSPORT.csv")
    x1 <- data.frame(x1[-1, ])
    x1 <- separate_wider_delim(x1, cols = "x1..1...", delim = ",", names = c("ENDUSE", "COUNTRY","FUEL","METRIC","TIME", "ACTIVITY", "ENERGY", "EMISSIONS", "INDICATOR"))
    x1 <- x1 %>% pivot_longer(!c("COUNTRY", "ENDUSE", "FUEL", "METRIC", "TIME"), names_to = "variable", values_to = "value")
    x1["sector"] <- "TRANSPORT"
    
    x2 <- read.csv2("EEI_RESIDENTIAL.csv")
    x2 <- data.frame(x2[-1, ])
    x2 <- separate_wider_delim(x2, cols = "x2..1...", delim = ",", names = c("ENDUSE", "COUNTRY","FUEL","METRIC","TIME", "ACTIVITY", "ENERGY", "EMISSIONS", "INDICATOR"))
    x2 <- x2 %>% pivot_longer(!c("COUNTRY", "ENDUSE", "FUEL", "METRIC", "TIME"), names_to = "variable", values_to = "value")
    x2["sector"] <- "RESIDENTIAL"
    
    x3 <- read.csv2("EEI_SERVICES.csv")
    x3 <- data.frame(x3[-1, ])
    x3 <- separate_wider_delim(x3, cols = "x3..1...", delim = ",", names = c("ENDUSE", "COUNTRY","FUEL","METRIC","TIME", "ACTIVITY", "ENERGY", "EMISSIONS", "INDICATOR"))
    x3 <- x3 %>% pivot_longer(!c("COUNTRY", "ENDUSE", "FUEL", "METRIC", "TIME"), names_to = "variable", values_to = "value")
    x3["sector"] <- "SERVICES"
    
    x4 <- read.csv2("EEI_INDUSTRY.csv")
    x4 <- data.frame(x4[-1, ])
    x4 <- separate_wider_delim(x4, cols = "x4..1...", delim = ",", names = c("ENDUSE", "COUNTRY","FUEL","METRIC","TIME", "ACTIVITY", "ENERGY", "EMISSIONS", "INDICATOR"))
    x4 <- x4 %>% pivot_longer(!c("COUNTRY", "ENDUSE", "FUEL", "METRIC", "TIME"), names_to = "variable", values_to = "value")
    x4["sector"] <- "INDUSTRY"
    
    x <- rbind(x1, x2, x3, x4)
    
    names(x) <- c("enduse", "region", "fuel", "metric" ,"period", "variable", "value", "sector")
    
    x[["region"]] <- factor(x[["region"]])
    x[["enduse"]] <- factor(x[["enduse"]])
    x[["fuel"]] <- factor(x[["fuel"]])
    x[["metric"]] <- factor(x[["metric"]])
    x[["variable"]] <- factor(x[["variable"]])
    x[["sector"]] <- factor(x[["sector"]])
    x[["period"]] <- as.numeric(x[["period"]])
    x[["value"]] <- as.numeric(x[["value"]])
    saveRDS(object = x, file = "IEA_Energy_End_uses_and_Efficiency_Indicators.rds")
  }
  
  x <- readRDS("IEA_Energy_End_uses_and_Efficiency_Indicators.rds")
  
  x <- filter(x, !is.na(x[["region"]]))
  if (subtype != "all") {
    x <- filter(x, x[["fuel"]] == subtype)
  }
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "Energy end-uses and efficiency indicators",
                       type = "Energy End-uses and Efficiency Indicators",
                       filename = "IEA_Energy_End_uses_and_Efficiency_Indicators.rds",
                       `Indicative size (MB)` = 17,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "E3M"))
}
