#' readTREMOVE
#'
#' Read TREMOVE data :
#' Reference has national policies with accepted measures until end-2019. REF2020_v6
#' Mix is a decarb scenario contributing to the EU Green Deal goals towards net zero in 2050. MIX_2021_v1
#'
#' @param subtype Type of data that should be read. The type is referring to the
#' excel and converts it to a magpie object.
#' 
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("TREMOVE", subtype = "AT_TRANSPORT_REF2020_v6")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter %>% select
#' @importFrom readxl read_excel
#'

readTREMOVE <- function(subtype = "AT_TRANSPORT_REF2020_v6") {
  

  x<-NULL
  
  for (i in list.files(".")) {
    
    x1 <- read_excel(i, sheet = "Activity")
    
    x1 <- x1[-c(1:6,28,33:36,53,58:60,77,82,83),-c(2:4,19)]
    
    names(x1)[1] <- "variable"
    
    x1 <- x1 %>% pivot_longer(!"variable", names_to = "period", values_to = "value")
    
    x1["technology"] <- NA
    x1[1 : 500, 4] <- "Transport activity"
    x1[501 : 900, 4] <- "Urban Transport activity per category"
    x1[901 : 1300, 4] <- "Inter-Urban Transport activity per category"
    x1["scenario"] <- "REF2020_v6"
    
    x1["sector"] <- NA
    
    x1[c(1:420,501:820,901:1220), 6] <- "Passenger transport activity (Gpkm)"
    x1[which(is.na(x1["sector"])), 6] <- "Freight transport activity (Gtkm)"
    
    x1["region"] <- substr(i, 1,2)
    
    x1[["region"]] <- toolCountry2isocode((x1[["region"]]))
    
    x1 <- as.quitte(x1)
    
    x1 <- filter(x1, !is.na(x1[["period"]]))
    
    x1 <- as.quitte(x1) %>% as.magpie()
    
    x <- rbind(x1,x)
    
  }
  

  
  list(x = x,
       weight = NULL,
       description = c(category = "model meta information, scenario meta information",
                       type = "model meta information, scenario meta information",
                       filename = "AEO_2017.xlsx",
                       `Indicative size (MB)` = 2.7,
                       dimensions = "3D",
                       unit = "varios",
                       Confidential = "E3M"))
}