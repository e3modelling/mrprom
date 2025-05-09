#' readPrimes
#'
#' Read Primes data :
#' 
#' @return The read-in data into a magpie object.
#'
#' @author Anastasis Giannousakis, Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("Primes")
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr filter %>% select
#' @importFrom readxl read_excel
#'

readPrimes <- function() {
  
  x<-NULL
  
  subtype = c("CIS", "CNF")
  var = c("IS", "NF")
  
  for (i in list.files(".")) {
    for (ex_sheet in subtype) {
      x1 <- read_excel(i, sheet = ex_sheet)
        
      x1 <- x1[c(2:3,5:39),c(1,3:18)]
      
      names(x1) <- x1[1,]
      
      names(x1)[1] <- "fuel"
      
      x1 <- x1[-1,]
      
      x1 <- x1 %>% pivot_longer(!"fuel", names_to = "period", values_to = "value")
      
      x1["variable"] <- var[which(subtype == ex_sheet)]
      
      x1["region"] <- substr(i, 2,3)
      
      if (i=="VEU28REF2020_v3bal.xlsx") {
        x1["region"] <- "EU28"
      }else if (i=="VEU27REF2020_v3bal.xlsx") {
        x1["region"] <- "EU27"
      }else if (i=="VEU12REF2020_v3bal.xlsx") {
        x1["region"] <- "EU12"
      }else if (i=="VEU15REF2020_v3bal.xlsx") {
        x1["region"] <- "EU15"
      }else if (i=="VEU27noUKREF2020_v3bal.xlsx") {
        x1["region"] <- "EU27noUK"
      }
      
      x1[["region"]] <- toolCountry2isocode(x1[["region"]], mapping =
                                              c("EU28" = "EU28",
                                                "EU27" = "EU27",
                                                "EU12" = "EU12",
                                                "EU15" = "EU15",
                                                "EU27noUK" = "EU27noUK",
                                                "EL" = "GRC"))
      
      x1["scenario"] <- substr(list.files(".")[1], 4,10)
      
      x1 <- as.quitte(x1)
      
      x1[["unit"]] <- "ktoe"
      
      x1 <- filter(x1, !is.na(x1[["region"]]))
      
      x <- rbind(x1,x)
    }
  }

  
  x <- as.quitte(x)
  x <- as.magpie(x)
  
  list(x = x,
       weight = NULL,
       description = c(category = "Final energy consumption",
                       type = "Final energy consumption",
                       filename = "VATREF2020_v3bal.xlsx",
                       `Indicative size (MB)` = 30,
                       dimensions = "4D",
                       unit = "various",
                       Confidential = "E3M"))
}