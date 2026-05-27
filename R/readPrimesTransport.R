#' readPrimesTransport
#'
#' Read PrimesTransport data :
#'
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas, Michael Madianos
#'
#' @examples
#' \dontrun{
#' a <- readSource("PrimesTransport")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter %>% select
#' @importFrom readxl read_excel

readPrimesTransport <- function() {
  files <- list.files(".")
  mapping <- "FuelOutlook"
  
  x <- NULL
  for (i in files) {
    x1 <- lapply(mapping, function(sheet) {
      x1 <- readSheet2(i, sheet, mapping, files)
      return(x1)
    })
    x <- mbind(x, do.call(mbind, x1))
  }
  list(
    x = x,
    weight = NULL,
    description = c(
      category = "Final energy consumption TRANSPORT",
      type = "Final energy consumption TRANSPORT",
      filename = "AT_TRANSPORT_REF2020_v3.xlsx",
      `Indicative size (MB)` = 30,
      dimensions = "4D",
      unit = "various",
      Confidential = "E3M"
    )
  )
}

# Helper ------------------------------------------------------------------------------------
readSheet2 <- function(excel_name, ex_sheet, map, files) {
  x1 <- read_excel(excel_name, sheet = ex_sheet)
  x1 <- x1[c(1,6,9,12,15:17,19,23,24,30,117,121,122:124,126:128,132,136,140,144:153,
             156,160,161:163,165:167,171,175,179,183:192,351,355,356:358,
             360:362,366,370,374,378:387,507,511,512:514,516:518,
             522,526,530,534:543,547,551,552:554,556:558,562,566,570,574:583,
             821,825,826:828,830:832,836,840,844,848:857,860,864,865:867,869:871,
             875,879,883,887:896,1016,1020,1021:1023,1025:1027,1031,1035,1039,
             1043:1052,1055,1059,1060:1062,1064:1066,1070,1074,1078,1082:1091,
             1288,1292,1293:1295,1297:1299,1303,1307,1311,1315:1324,
             1327,1331,1332:1334,1336:1338,1342,1346,1350,1354:1363,
             1366,1370,1371:1373,1375:1377,1381,1385,1389,1393:1402),-c(2,17:23)]
  names(x1) <- x1[1, ]
  names(x1)[1] <- "variable"
  x1 <- x1[-1, ]
  x1 <- x1 %>% pivot_longer(!"variable", names_to = "period", values_to = "value")
  
  x1["technology"] <- NA
  x1[1 : 42, 4] <- "Road transport"
  x1[43 : 56, 4] <- "Trucks"
  x1[57 : 70, 4] <- "Passenger Light Duty Vehicles"
  x1[71 : 84, 4] <- "Freight Light Duty Vehicles"
  x1[85 : 112, 4] <- "Rail"
  x1[113 : 126, 4] <- "Aviation"
  x1[127 : 140, 4] <- "Inland navigation"
  x1[141 : 434, 4] <- "Public road transport"
  x1[435 : 728, 4] <- "Buses"
  x1[729 : 1022, 4] <- "Private cars"
  x1[1023 : 1316, 4] <- "Passenger Light Duty Vehicles"
  x1[1317 : 1610, 4] <- "Trucks"
  x1[1611 : 1904, 4] <- "Rail"
  x1[1905 : 2198, 4] <- "Passenger trains"
  x1[2199 : 2492, 4] <- "Freight trains"
  x1[2493 : 2786, 4] <- "Aviation"
  x1[2787 : 3080, 4] <- "Inland navigation"
  x1[3081 : 3374, 4] <- "Passenger inland navigation"
  x1[3375 : 3668, 4] <- "Freight inland navigation"
  x1["sector"] <- "Final Energy Demand (in ktoe)"
  
  x1["region"] <- substr(excel_name, 1, 2)
  if (excel_name == "EU28_TRANSPORT_REF2020_v3.xlsx") {
    x1["region"] <- "EU28"
  } else if (excel_name == "EU27_TRANSPORT_REF2020_v3.xlsx") {
    x1["region"] <- "EU27"
  } else if (excel_name == "EU12_TRANSPORT_REF2020_v3.xlsx") {
    x1["region"] <- "EU12"
  } else if (excel_name == "EU15_TRANSPORT_REF2020_v3.xlsx") {
    x1["region"] <- "EU15"
  } else if (excel_name == "EU27noUK_TRANSPORT_REF2020_v3.xlsx") {
    x1["region"] <- "EU27noUK"
  }
  
  suppressWarnings({
    x1[["region"]] <- toolCountry2isocode(x1[["region"]],
                                          mapping =
                                            c(
                                              "EU28" = "EU28",
                                              "EU27" = "EU27",
                                              "EU12" = "EU12",
                                              "EU15" = "EU15",
                                              "EU27noUK" = "EU27noUK",
                                              "EL" = "GRC"
                                            )
    )
  })

  x1["scenario"] <- substr(files[1], 14, 20)
  x1 <- as.quitte(x1)
  x1[["unit"]] <- "ktoe"
  x1 <- filter(x1, !is.na(x1[["region"]]))
  x1 <- as.quitte(x1)
  x1 <- as.magpie(x1)
  return(x1)
}
