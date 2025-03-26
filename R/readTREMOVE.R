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
    
    x1 <- x1[-c(1:7,10,13,18:21,28,35,47,50,57,68:71,76,
                80:81,83:85,92,99),-c(2:4,19:25)]
    
    names(x1)[1] <- "variable"
    
    x1 <- x1 %>% pivot_longer(!"variable", names_to = "period", values_to = "value")
    
    x1["technology"] <- NA
    x1[1 : 28, 4] <- "Public road transport"
    x1[29 : 56, 4] <- "Private cars"
    x1[57 : 84, 4] <- "2wheelers"
    x1[85 : 98, 4] <- "Heavy duty vehicles"
    x1[99 : 112, 4] <- "Light Duty vehicles"
    x1[113 : 196, 4] <- "Buses"
    x1[197 : 280, 4] <- "Coaches"
    x1[281 : 434, 4] <- "Private cars"
    x1[435 : 462, 4] <- "2wheelers"
    x1[463 : 546, 4] <- "Heavy duty vehicles"
    x1[547 : 686, 4] <- "Light duty vehicles"
    x1[687 : 728, 4] <- "Passenger "
    x1[729 : 742, 4] <- "Passenger "
    x1[743 : 784, 4] <- "Freight"
    x1[785 : 798, 4] <- "AVIATION"
    x1[799 : 882, 4] <- "Passenger"
    x1[883 : 966, 4] <- "Freight"
    
    x1["sector"] <- NA
    x1[1 : 112, 5] <- "ROAD TRANSPORT Total Stock (in thousand vehicles)"
    x1[113 : 686, 5] <- "ROAD TRANSPORT Total stock per category and per fuel (in thousand vehicles)"
    x1[687 : 784, 5] <- "RAIL TRANSPORT"
    x1[785 : 798, 5] <- "AVIATION"
    x1[799 : 966, 5] <- "NAVIGATION"
    
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