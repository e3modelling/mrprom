#' readTREMOVE
#'
#' Read TREMOVE data :
#' Reference has national policies with accepted measures until end-2019. REF2020_v6
#' Mix is a decarb scenario contributing to the EU Green Deal goals towards net zero in 2050. MIX_2021_v1
#'
#' @param subtype Type of data that should be read. The type is referring to the
#' excel sheet, and convert it to a magpie object.
#' Available types are:
#' \itemize{
#' \item `Activity`:
#' \item `Stock`:
#' \item `FuelOutlook`:
#' }
#' 
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("TREMOVE", subtype = "Activity")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter %>% select
#' @importFrom readxl read_excel
#'

readTREMOVE <- function(subtype = "Activity") {
  

  x<-NULL
  
  if (subtype == "Activity") {
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
      
      x1["scenario"] <- substr(i, 14,16)
      
      if (unique(x1[["region"]]) == "EU") {
        x1["scenario"] <- substr(i, 20,22)
      }
      
      x1 <- as.quitte(x1)
      
      x1["unit"] <- "Gkm"
      
      x1 <- filter(x1, !is.na(x1[["region"]]))
      
      x <- rbind(x1,x)
      
    }
  }
  
  if (subtype == "Stock") {
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
      
      x1["scenario"] <- substr(i, 14,16)
      
      if (unique(x1["region"]) == "EU") {
        x1["scenario"] <- substr(i, 20,22)
      }
      
      x1 <- as.quitte(x1)
      
      x1["unit"] <- "thousand vehicles"
      
      x1 <- filter(x1, !is.na(x1[["region"]]))
      
      x <- rbind(x1,x)
    }
  }
  
  
  if (subtype == "FuelOutlook") {
    #sheet = "FuelOutlook"
    for (i in list.files(".")) {
      
      x1 <- read_excel(i, sheet = "FuelOutlook")
      
      x1 <- x1[c(6,9,12,15:17,19,23,24,30,117,121,122,126:128,132,136,140,144:153,
                 351,355,356,360:362,366,370,374,378:387,547,551,552,556:558,562,
                 566,570,574:583,821,825,826,830:832,836,840,844,848:857,1016,1020,
                 1021,1025:1027,1031,1035,1039,1043:1052,1055,1059,1060,1064:1066,
                 1070,1074,1078,1082:1091,1288,1292,1293,1297:1299,1303,1307,1311,
                 1315:1324),-c(2,17:23)]
      
      names(x1)[1] <- "variable"
      
      x1 <- x1 %>% pivot_longer(!"variable", names_to = "period", values_to = "value")
      
      x1["technology"] <- NA
      x1[1 : 42, 4] <- "Road transport"
      x1[43 : 56, 4] <- "Trucks"
      x1[57 : 70, 4] <- "Passenger Light Duty Vehicles"
      x1[71 : 84, 4] <- "Freight Light Duty Vehicles"
      x1[85 : 112, 4] <- "Rail"
      x1[113 : 126, 4] <- "Aviation"
      x1[127 : 140, 4] <- "Inland navigation"
      
      x1[141 : 406, 4] <- "Public road transport"
      
      x1[407 : 672, 4] <- "Private cars"
      
      x1[673 : 938, 4] <- "Trucks"
      
      x1[939 : 1204, 4] <- "Rail"
      
      x1[1205 : 1470, 4] <- "Freight trains "
      
      x1[1471 : 1736, 4] <- "Aviation"
      
      x1[1737 : 2002, 4] <- "Inland navigation"
      
      
      
      x1["sector"] <- "Final Energy Demand (in ktoe)"
      
      x1["region"] <- substr(i, 1,2)
      
      x1[["region"]] <- toolCountry2isocode(x1[["region"]], mapping =
                                              c("EU" = "EU"))
      
      x1["scenario"] <- substr(i, 14,16)
      
      if (unique(x1["region"]) == "EU") {
        x1["scenario"] <- substr(i, 20,22)
      }
      
      x1 <- as.quitte(x1)
      
      x1["unit"] <- "ktoe"
      
      x1 <- filter(x1, !is.na(x1[["region"]]))
      
      x <- rbind(x1,x)
    }
  }

  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "Transport",
                       type = "Activity, Stock, FuelOutlook",
                       filename = "IT_TRANSPORT_REF2020_v6.xlsx",
                       `Indicative size (MB)` = 0.9,
                       dimensions = "4D",
                       unit = "various",
                       Confidential = "E3M"))
}