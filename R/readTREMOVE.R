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
  
  #sheet = "Activity"
  for (i in list.files(".")) {
    
    x1 <- read_excel(i, sheet = "Activity")
    
    x1 <- x1[-c(1:7,10,14,17,21,28,33:37,40,44,47,53,58:61,64,68,71,77,82,83),-c(2:4,19:25)]
    
    names(x1)[1] <- "variable"
    
    x1 <- x1 %>% pivot_longer(!"variable", names_to = "period", values_to = "value")
    
    x1["technology"] <- NA
    x1[1 : 280, 4] <- "Transport activity"
    x1[281 : 504, 4] <- "Urban Transport activity per category"
    x1[505 : 728, 4] <- "Inter-Urban Transport activity per category"
    
    x1["sector"] <- NA
    x1[c(1:224, 281:448, 505:672), 5] <- "Passenger transport activity (Gpkm)"
    x1[c(225:280, 449:504, 672:728), 5] <- "Freight transport activity (Gtkm)"
    
    x1["region"] <- substr(i, 1,2)
    
    x1[["region"]] <- toolCountry2isocode(x1[["region"]], mapping =
                                            c("EU" = "EU"))
    
    x1["scenario"] <- substr(list.files(".")[1], 14,23)
    
    x1 <- as.quitte(x1)
   
    x1 <- filter(x1, !is.na(x1[["region"]]))
    
    x <- rbind(x1,x)
    
  }
  
  #sheet = "Stock"
  for (i in list.files(".")) {
    
    x1 <- read_excel(i, sheet = "Stock")
    
    x1 <- x1[-c(1:7,10,13,18:20,22:27,29:34,36:46,48:49,51:56,58:69,71:75,77:80,
                82:83,85:91,93:99),-c(2:4,19:25)]
    
    names(x1)[1] <- "variable"
    
    x1 <- x1 %>% pivot_longer(!"variable", names_to = "period", values_to = "value")
    
    x1["technology"] <- NA
    x1[1 : 112, 4] <- "ROAD TRANSPORT (in thousand vehicles)"
    x1[113 : 196, 4] <- "Total stock per category (in thousand vehicles)"
    x1[197 : 224, 4] <- "RAIL TRANSPORT"
    x1[225 : 238, 4] <- "AVIATION"
    x1[239 : 266, 4] <- "NAVIGATION"
    
    x1["sector"] <- NA
    x1[c(197:210, 239:252), 5] <- "Passenger"
    x1[c(211:224, 253:266), 5] <- "Freight"
    
    x1["region"] <- substr(i, 1,2)
    
    x1[["region"]] <- toolCountry2isocode(x1[["region"]], mapping =
                                            c("EU" = "EU"))
    
    x1["scenario"] <- substr(list.files(".")[1], 14,23)
    
    x1 <- as.quitte(x1)
    
    x1 <- filter(x1, !is.na(x1[["region"]]))
    
    x <- rbind(x1,x)
    
  }
  
  x <- as.magpie(x)
  
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